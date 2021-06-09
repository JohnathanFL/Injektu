using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Threading;
using System.Linq;
using System.Reflection;
using Injektu.Utils;
using System.Diagnostics;

namespace Injektu
{
    /// <summary>
    /// Indicates that this property should be resolved via injection. <br />
    /// Feel free to subclass this and add more information to it. Container will scan for properties with attributes that
    /// are simply *assignable* to this type.
    /// </summary>
    [AttributeUsage(AttributeTargets.Property)]
    public class InjectAttribute : Attribute
    {
        public bool Required { get; init; } = true;
    }

    [AttributeUsage(AttributeTargets.Property)]
    public class InjectByNameAttribute : InjectAttribute
    {
        /// <summary>
        /// Must either be (in the case TKey == string) or parse out to (TKey.Parse(Key)) the key for this instance.
        /// </summary>
        /// <value></value>
        public string? Key { get; init; }
    }

    // TODO
    public enum ServiceScope
    {
        /// <summary>
        /// Will only ever construct one instance each time it's registered.
        /// To get a new one, re-register it.
        /// </summary>
        Singleton,
        /// <summary>
        /// Will create one per <see cref="Container" />. Just a shorthand for re-registering this instance
        /// with each child scope, instead of registering a new singleton each time.
        /// </summary>
        Scoped,
        /// <summary>
        /// Makes a new one each time it's resolved.
        /// </summary>
        Transient
    }

    public record ContainerByType(string Name, Container<object, Type, InjectAttribute>? Parent) : ContainerByType<object>(Name, Parent);

    public record ContainerByType<TBase>(string Name, Container<object, Type, InjectAttribute>? Parent) : Container<object, Type, InjectAttribute>(Name, Parent)
    {
        protected override Type GetKeyFromProperty(PropertyInfo prop) => prop.PropertyType;

        public void Register<TService>(Expression<Func<TService>> ctor, ServiceScope scope) where TService : TBase => Register(typeof(TService), ctor, scope);

        public TService? Resolve<TService>(bool required = true) where TService : class, TBase => Resolve(typeof(TService), required) as TService;
    }

    /// <summary>
    /// A container
    /// </summary>
    /// <param name="Name">Name this scope. Literally only part of the spec because C# is stupid, and not having it would conflict with the copy constructor.</param>
    /// <param name="Parent"></param>
    /// <param name="TBase">The required base class for all services</param>
    /// <param name="TKey">The type used to identify keys</param>
    /// <returns></returns>
    public abstract record Container<TBase, TKey, TMarker>(string Name, Container<TBase, TKey, TMarker>? Parent)
        where TKey : notnull
        where TMarker : InjectAttribute
    {
        public bool Sealed { get; private set; } = Parent?.Sealed == true ? throw new InvalidOperationException("A ContainerScope's parent must be already sealed.") : false;

        public TObj ResolveOn<TObj>(TObj obj) where TObj : notnull => (TObj)ResolveOn(obj, typeof(TObj));

        /// <summary>
        /// Perform property resolution on an already instantiated object.
        /// </summary>
        /// <param name="obj">Pre-instantiated object to instantiate.</param>
        /// <returns>The parameter. Will be the same instance.</returns>
        public object ResolveOn(object obj, Type t)
        {
            if (!Sealed) throw new InvalidOperationException("A Container must be sealed before attempting any resolutions.");

            if (Resolvers.TryGetValue(t, out var resolver)) return resolver(obj);

            resolver = MakeResolver(t);
            Resolvers[t] = resolver;
            return resolver(obj);
        }

        /// <summary>
        /// Resolve a service by overarching type.
        /// </summary>
        /// <returns>Null if no service has been registered for this type.</returns>
        public TBase? Resolve(TKey key, bool require = true) => Sealed
            ? Creators.TryGetValue(key, out var creator)
                ? creator()
                : require
                    ? throw new InvalidOperationException($"Service {key} not registered with container {Name}")
                    : default
            : throw new InvalidProgramException("May only resolve after sealing.");

        /// <summary>
        /// Bind a constructor to a service type. Will overwrite any previously bound service on that TService.
        /// </summary>
        /// <param name="ctor"></param>
        /// <param name="scope"></param>
        /// <param name="name">If specified, will also bind this service to a name at the same time.</param>
        public void Register<TService>(TKey key, Expression<Func<TService>> ctor, ServiceScope scope)
            where TService : TBase
        {
            if (Sealed) throw new InvalidProgramException("May only register before sealing!");

            var t = typeof(TService);

            var scopeParam = Expression.Parameter(typeof(Container<TBase, TKey, TMarker>), "scope");
            var retLbl = Expression.Label();
            var innerCtor = Expression.Lambda<Func<Container<TBase, TKey, TMarker>, TBase>>(Expression.Call(scopeParam, ResolveOnInfo, new Expression[] { ctor.Body, Expression.Constant(t) }), new[] { scopeParam });

            Scopes[key] = scope;
            Ctors[key] = innerCtor;
        }

        /// <summary>
        /// Seals the scope, allowing no further registration and signaling that we may now bake all constructors at the bytecode level.
        /// Resolves prior to this and registers after this will throw <see cref="InvalidOperationException" />.
        /// </summary>
        public void Seal()
        {
            if (Sealed) throw new InvalidOperationException("Cannot re-seal a sealed ContainerScope!");
            Sealed = true;

            CacheTransients();
            CacheInstances();
            CacheCreators();
        }

        protected abstract TKey GetKeyFromProperty(PropertyInfo prop);

        internal static MethodInfo ResolveOnInfo = typeof(Container<TBase, TKey, TMarker>).GetMethods().Single(m => m.Name == nameof(ResolveOn) && m.GetParameters().Length == 2);

        internal Func<object, object> MakeResolver(Type t)
        {
            // Note that we are an object => object. We unbox the object into a `t` at the start, then re-box
            // to an object at the end.

            // The value we're placing the resolved values on.
            var tgtParam = Expression.Parameter(typeof(TBase), "target");
            var stmts = new List<Expression>();
            var tgt = Expression.Variable(t, "tgt");
            stmts.Add(Expression.Assign(tgt, Expression.Convert(tgtParam, t)));
            // stmts.Add(Expression.Invoke(Expression.Constant((Action<object>)(_ => Console.WriteLine(new StackTrace()))), tgt));
            var retLblTgt = Expression.Label(typeof(TBase));
            var retLbl = Expression.Label(retLblTgt, tgtParam);

            // We inject any property marked `[Inject]` *or any other attribute descended from [Inject]*.
            foreach (var prop in from prop in t.GetProperties() 
                                 where prop.GetCustomAttribute<TMarker>() is not null 
                                 select prop)
            {
                Expression injected = null!;
                var ty = prop.PropertyType;

                var key = GetKeyFromProperty(prop);

                if (!Scopes.ContainsKey(key)) injected = Expression.Default(ty);
                else
                {
                    // if (!trans.ContainsKey(key)) Console.WriteLine(new StackTrace());
                    injected = Scopes[key] switch
                    {
                        ServiceScope.Singleton or ServiceScope.Scoped => Expression.Constant(Instances[key], ty),
                        ServiceScope.Transient => Expression.Invoke(Expression.Constant(Transients[key])),
                        _ => throw new InvalidProgramException("This can never happen, Microsoft. Shut it."),
                    };
                }


                stmts.Add(Expression.Assign(
                    Expression.PropertyOrField(tgt, prop.Name),
                    Expression.Convert(injected, ty)
                ));
            }

            stmts.Add(Expression.Return(retLblTgt, Expression.Convert(tgt, typeof(object)), typeof(object)));

            stmts.Add(retLbl);

            // Console.WriteLine($"Made a resolver for {t}:");
            // foreach (var stmt in stmts)
            // {
            //     Console.WriteLine("\t" + stmt);
            // }

            var lambda = Expression.Lambda<Func<object, object>>(Expression.Block(new[] { tgt }, stmts), false, new[] { tgtParam });

            return lambda.Compile();
        }

        internal void CacheInstances()
        {
            // Copy any singletons from the parent, if applicable. Scopes must be re-created.
            if (Parent is not null)
                foreach (var (key, inst) in Parent.Instances.Where(_ => Scopes[_.Key] == ServiceScope.Singleton))
                    Instances[key] = inst;
            foreach (var (key, ctor) in Ctors.Where(_ => Scopes[_.Key] != ServiceScope.Transient))
            {
                // Console.WriteLine($"Caching instance for {key} which is a {scopes[key]}");
                var inst = ctor.Compile().Invoke(this);
                if (inst is null) throw new InvalidProgramException($"Provided a null-constructor for {key} with scope {Scopes[key]}");

                Instances[key] = inst;
            }
        }

        internal void CacheTransients()
        {
            foreach (var (key, ctor) in Ctors.Where(_ => Scopes[_.Key] == ServiceScope.Transient))
            {
                var method = (Func<Container<TBase, TKey, TMarker>, TBase>)ctor.Compile();
                Transients[key] = () => method(this);
                // Console.WriteLine($"Added transient {key}");
            }
        }

        /// <summary>
        /// As opposed to <see cref="MakeResolver(Type)" />, this caches the methods used directly by the Resolve() family of methods.
        /// </summary>
        internal void CacheCreators()
        {
            foreach (var (key, ctor) in Ctors.Where(_ => Scopes[_.Key] == ServiceScope.Transient))
            {
                var lambda = Expression.Lambda<Func<TBase>>(Expression.Invoke(ctor, Expression.Constant(this)));
                var func = lambda.Compile();
                // creators[key] = () => { Console.WriteLine($"Doing creator for transient {key}"); return func(); };
                Creators[key] = func;
                // Console.WriteLine($"Cached creator for {key}: {lambda}");
            }

            foreach (var (key, inst) in Instances)
                // creators[key] = () => { Console.WriteLine($"Doing instance for {key}"); return inst; };
                Creators[key] = () => inst;

        }


        internal readonly Dictionary<TKey, ServiceScope> Scopes = Parent?.Scopes.CloneDictionary() ?? new();

        internal readonly Dictionary<TKey, Expression<Func<Container<TBase, TKey, TMarker>, TBase>>> Ctors = Parent?.Ctors.CloneDictionary() ?? new();

        // ==============================================================================================
        // | Anything above this line is fair game for copying to a new instance (and should be copied) |
        // ==============================================================================================

        internal readonly Dictionary<TKey, TBase> Instances = new();

        internal readonly Dictionary<TKey, Func<TBase>> Transients = new();

        internal readonly Dictionary<TKey, Func<TBase>> Creators = new();

        /// <summary>
        /// Baked functions that assign relevant services to their appropriate properties.
        /// </summary>
        internal readonly Dictionary<Type, Func<object, object>> Resolvers = new();
    }
}
