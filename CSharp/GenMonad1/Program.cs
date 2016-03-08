using System;
using System.Linq;
using FsCheck;
using FsCheck.Fluent;

namespace GenMonad1
{
    internal static class Program
    {
        private static void Main()
        {
            var g1 =
                from s in Arb.generate<string>()
                where !string.IsNullOrEmpty(s)
                from c in Gen.elements(s)
                select Tuple.Create(s, c);

            foreach (var sample in Gen.sample(10, 10, g1)) Console.WriteLine(sample);

            Console.WriteLine(new string('-', 80));

            var g2 =
                from s in Any.OfType<string>()
                where !string.IsNullOrEmpty(s)
                from c in Any.ValueIn(s.AsEnumerable())
                select Tuple.Create(s, c);

            foreach (var sample in Gen.sample(10, 10, g2)) Console.WriteLine(sample);
        }
    }
}
