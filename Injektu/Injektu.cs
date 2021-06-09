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
    /// Maps directly to either <see cref="Container.Resolve{TService}" /> or <see cref="Container.Resolve{TService}(string)" />, depending on
    /// whether <see cref="InjectedAttribute.Name" /> has been set or not.
    /// </summary>
    [AttributeUsage(AttributeTargets.Property | AttributeTargets.Field)]
    public class InjectedAttribute : Attribute
    {
        public string? Name { get; init; }

        public bool Required { get; init; } = true;
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

    /// <summary>
    /// A container
    /// </summary>
    /// <param name="Name">Name this scope. Literally only part of the spec because C# is stupid, and not having it would conflict with the copy constructor.</param>
    /// <param name="Parent"></param>
    /// <returns></returns>
    public record Container(string Name, Container? Parent)
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
        public TService? Resolve<TService>(bool require = true) => Sealed 
            ? TypeCreators.TryGetValue(typeof(TService), out var creator) 
                ? (TService?)creator() 
                : require 
                    ? throw new InvalidOperationException($"Service {typeof(TService).Name} not registered with container {Name}") 
                    : default 
            : throw new InvalidProgramException("May only resolve after sealing.");

        /// <summary>
        /// Resolve a service purely by name. See <see cref="Register{TService}(Expression{Func{TService}}, ServiceScope, string?)" /> for more details.
        /// </summary>
        public TService? Resolve<TService>(string name, bool require = true) => Sealed 
            ? NameCreators.TryGetValue(name, out var creator) 
                ? (TService?)creator() 
                : require 
                    ? throw new InvalidOperationException($"Service {name} not found in context {Name}") 
                    : default 
            : throw new InvalidProgramException("May only resolve after sealing");

        /// <summary>
        /// Bind a constructor to a service type. Will overwrite any previously bound service on that TService.
        /// </summary>
        /// <param name="ctor"></param>
        /// <param name="scope"></param>
        /// <param name="name">If specified, will also bind this service to a name at the same time.</param>
        public void RegisterByType<TService>(Expression<Func<TService>> ctor, ServiceScope scope, string? name = null)
        {
            if (Sealed) throw new InvalidProgramException("May only register before sealing!");

            var t = typeof(TService);

            var scopeParam = Expression.Parameter(typeof(Container), "scope");
            var retLbl = Expression.Label();
            var innerCtor = Expression.Lambda(Expression.Call(scopeParam, ResolveOnInfo, new Expression[] { ctor.Body, Expression.Constant(t) }), new[] { scopeParam });

            TypeScopes[t] = scope;
            TypeCtors[t] = innerCtor;
            if (name is not null)
            {
                ServiceNames[name] = t;
                NameScopes[name] = scope;
                NameCtors[name] = innerCtor;
            }
        }

        /// <summary>
        /// Bind a constructor to a service name. Identical to <see cref="Register{TService}(Expression{Func{TService}}, ServiceScope, string?)" /> except
        /// that this method will not allow resolution by TService. All other semantics are identical to that function.
        /// </summary>
        public void RegisterByName<TObj>(Expression<Func<TObj>> ctor, string name, ServiceScope scope)
        {
            var t = typeof(TObj);

            var scopeParam = Expression.Parameter(typeof(Container), "scope");
            var retLbl = Expression.Label();
            var innerCtor = Expression.Lambda(Expression.Call(scopeParam, ResolveOnInfo, new Expression[] { ctor.Body, Expression.Constant(t) }), false, new[] { scopeParam });
            (NameScopes[name], NameCtors[name]) = (scope, innerCtor);
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

        internal static MethodInfo ResolveOnInfo = typeof(Container).GetMethods().Single(m => m.Name == nameof(ResolveOn) && m.GetParameters().Length == 2);

        internal Func<object, object> MakeResolver(Type t)
        {
            // Note that we are an object => object. We unbox the object into a `t` at the start, then re-box
            // to an object at the end.

            // The value we're placing the resolved values on.
            var tgtParam = Expression.Parameter(typeof(object), "target");
            var stmts = new List<Expression>();
            var tgt = Expression.Variable(t, "tgt");
            stmts.Add(Expression.Assign(tgt, Expression.Convert(tgtParam, t)));
            // stmts.Add(Expression.Invoke(Expression.Constant((Action<object>)(_ => Console.WriteLine(new StackTrace()))), tgt));
            var retLblTgt = Expression.Label(typeof(object));
            var retLbl = Expression.Label(retLblTgt, tgtParam);
            foreach (var (prop, attrib) in t.GetProperties().Select(p => (p: p, a: p.GetCustomAttribute<InjectedAttribute>())).Where(pa => pa.a is not null))
            {
                Expression injected = null!;
                void getInjected<TKey>(PropertyInfo prop, Type ty, TKey key, Dictionary<TKey, ServiceScope> scopes, Dictionary<TKey, Func<object>> trans, Dictionary<TKey, object> insts)
                    where TKey : notnull
                {

                    if (!scopes.ContainsKey(key)) injected = Expression.Default(ty);
                    else
                    {
                        // if (!trans.ContainsKey(key)) Console.WriteLine(new StackTrace());
                        injected = scopes[key] switch
                        {
                            ServiceScope.Singleton or ServiceScope.Scoped => Expression.Constant(insts[key], ty),
                            ServiceScope.Transient => Expression.Invoke(Expression.Constant(trans[key])),
                            _ => throw new InvalidProgramException("This can never happen, Microsoft. Shut it."),
                        };
                    }
                }
                var ty = prop.PropertyType;

                if (attrib.Name is not null) getInjected<string>(prop, ty, attrib.Name, NameScopes, NameTransients, NameInstances);
                else getInjected<Type>(prop, ty, ty, TypeScopes, TypeTransients, TypeInstances);

                stmts.Add(Expression.Assign(
                    Expression.PropertyOrField(tgt, prop.Name),
                    Expression.Convert(injected, ty)
                ));
            }

            stmts.Add(Expression.Return(retLblTgt, Expression.Convert(tgt, typeof(object)), typeof(object)));

            stmts.Add(retLbl);

            Console.WriteLine($"Made a resolver for {t}:");
            foreach (var stmt in stmts)
            {
                Console.WriteLine("\t" + stmt);
            }

            var lambda = Expression.Lambda<Func<object, object>>(Expression.Block(new[] { tgt }, stmts), false, new[] { tgtParam });

            return lambda.Compile();
        }

        internal void CacheInstances()
        {
            void cache<TKey>(Dictionary<TKey, ServiceScope> scopes, Dictionary<TKey, object>? parentInsts, Dictionary<TKey, LambdaExpression> ctors, Dictionary<TKey, object> insts)
                where TKey : notnull
            {
                // Copy any singletons from the parent, if applicable. Scopes must be re-created.
                if (Parent is not null)
                    foreach (var (key, inst) in parentInsts!.Where(_ => scopes[_.Key] == ServiceScope.Singleton))
                        insts[key] = inst;
                foreach (var (key, ctor) in ctors.Where(_ => scopes[_.Key] != ServiceScope.Transient))
                {
                    Console.WriteLine($"Caching instance for {key} which is a {scopes[key]}");
                    var inst = ctor.Compile()?.DynamicInvoke(this);
                    if (inst is null) throw new InvalidProgramException($"Provided a null-constructor for {key} with scope {scopes[key]}");

                    insts[key] = inst;
                }
            }

            cache<Type>(TypeScopes, Parent?.TypeInstances, TypeCtors, TypeInstances);
            cache<string>(NameScopes, Parent?.NameInstances, NameCtors, NameInstances);
        }

        internal void CacheTransients()
        {
            void doCache<TKey>(Dictionary<TKey, ServiceScope> scopes, Dictionary<TKey, LambdaExpression> ctors, Dictionary<TKey, Func<object>> transients) where TKey : notnull
            {
                foreach (var (key, ctor) in ctors.Where(_ => scopes[_.Key] == ServiceScope.Transient))
                {
                    var method = (Func<Container, object>)ctor.Compile();
                    transients[key] = () => method(this);
                    Console.WriteLine($"Added transient {key}");
                }
            }

            doCache<string>(NameScopes, NameCtors, NameTransients);
            doCache<Type>(TypeScopes, TypeCtors, TypeTransients);
        }

        /// <summary>
        /// As opposed to <see cref="MakeResolver(Type)" />, this caches the methods used directly by the Resolve() family of methods.
        /// </summary>
        internal void CacheCreators()
        {
            void doCached<TKey>(Dictionary<TKey, ServiceScope> scopes, Dictionary<TKey, LambdaExpression> ctors, Dictionary<TKey, object> insts, Dictionary<TKey, Func<object>> creators) where TKey : notnull
            {
                foreach (var (key, ctor) in ctors.Where(_ => scopes[_.Key] == ServiceScope.Transient))
                {
                    var lambda = Expression.Lambda<Func<object>>(Expression.Invoke(ctor, Expression.Constant(this)));
                    var func = lambda.Compile();
                    // creators[key] = () => { Console.WriteLine($"Doing creator for transient {key}"); return func(); };
                    creators[key] = func;
                    // Console.WriteLine($"Cached creator for {key}: {lambda}");
                }

                foreach (var (key, inst) in insts)
                    // creators[key] = () => { Console.WriteLine($"Doing instance for {key}"); return inst; };
                    creators[key] = () => inst;

            }
            doCached<string>(NameScopes, NameCtors, NameInstances, NameCreators);
            doCached<Type>(TypeScopes, TypeCtors, TypeInstances, TypeCreators);
        }


        internal readonly Dictionary<Type, ServiceScope> TypeScopes = Parent?.TypeScopes.CloneDictionary() ?? new();

        internal readonly Dictionary<string, ServiceScope> NameScopes = Parent?.NameScopes.CloneDictionary() ?? new();

        /// <summary>
        /// When a service is registered by both type and name, we use this table to maintain that relationship so
        /// that resolution by both type and name return the same instance for singletons/scopees.
        /// 
        /// TODO
        /// </summary>
        internal readonly Dictionary<string, Type> ServiceNames = Parent?.ServiceNames.CloneDictionary() ?? new();

        internal readonly Dictionary<Type, LambdaExpression> TypeCtors = Parent?.TypeCtors.CloneDictionary() ?? new();

        internal readonly Dictionary<string, LambdaExpression> NameCtors = Parent?.NameCtors.CloneDictionary() ?? new();

        // =====================================================================================
        // |Anything above this line is fair game for copying to a new instance (and should be)|
        // =====================================================================================

        internal readonly Dictionary<Type, object> TypeInstances = new();
        internal readonly Dictionary<string, object> NameInstances = new();

        internal readonly Dictionary<Type, Func<object>> TypeTransients = new();
        internal readonly Dictionary<string, Func<object>> NameTransients = new();

        internal readonly Dictionary<Type, Func<object>> TypeCreators = new();
        internal readonly Dictionary<string, Func<object>> NameCreators = new();

        /// <summary>
        /// Baked functions that assign relevant services to their appropriate properties.
        /// </summary>
        internal readonly Dictionary<Type, Func<object, object>> Resolvers = new();
    }
}
