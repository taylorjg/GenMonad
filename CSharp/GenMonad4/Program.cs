using System;
using System.Linq;
using FsCheck;

namespace GenMonad4
{
    internal static class Program
    {
        private static void Main()
        {
            var g =
                from s in Arb.Generate<string>()
                where !string.IsNullOrEmpty(s)
                from c in Gen.Elements(s.AsEnumerable())
                select Tuple.Create(s, c);

            foreach (var sample in Gen.Sample(10, 10, g)) Console.WriteLine(sample);
        }
    }
}
