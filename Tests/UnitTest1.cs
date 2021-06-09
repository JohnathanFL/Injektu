using System;
using System.Diagnostics;
using Xunit;

namespace Injektu.Test
{
    public class Tests
    {
        public class Bar
        {
            public static int InstanceCount = 0;
            public int InstanceNum;

            public Bar()
            {
                InstanceNum = InstanceCount++;
                // Console.WriteLine($"Bar({InstanceNum})");
            }
        }

        public class Foo
        {
            public static int InstanceCount = 0;
            public int InstanceNum;

            [Inject]
            public Bar Bar { get; set; } = null;

            public Foo()
            {
                InstanceNum = InstanceCount++;
                // Console.WriteLine("Called Foo's ctor");
            }
        }

        [Fact]
        public void Test_ContainerByType()
        {
            var cont = new ContainerByType("root", null);
            cont.Register<Foo>(() => new(), ServiceScope.Singleton);
            cont.Register<Bar>(() => new(), ServiceScope.Transient);

            // Assert.Throws<InvalidProgramException>(() => cont.Resolve<Foo>());

            cont.Seal();

            var b1 = cont.Resolve<Bar>();
            var b2 = cont.Resolve<Bar>();
            Assert.NotNull(b1);
            Assert.NotNull(b2);

            Assert.NotEqual(b1.InstanceNum, b2.InstanceNum);

            var f1 = cont.Resolve<Foo>();
            var f2 = cont.Resolve<Foo>();

            Assert.NotNull(f1);
            Assert.NotNull(f2);

            Assert.Equal(f1.InstanceNum, f2.InstanceNum);

            Assert.NotNull(f1.Bar);
            Assert.NotNull(f2.Bar);

            Assert.Equal(f1.Bar.InstanceNum, f2.Bar.InstanceNum);
        }
    }
}
