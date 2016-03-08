using System;
using FsCheck;
using FsCheck.Fluent;

namespace GenMonad2
{
    internal static class Program
    {
        private static void Main()
        {
            var g1 =
                Any.OfType<string>()
                    .Where(s => !string.IsNullOrEmpty(s))
                    .SelectMany(s => Gen.elements(s)
                        .Select(c => Tuple.Create(s, c)));

            foreach (var sample in Gen.sample(10, 10, g1)) Console.WriteLine(sample);

            Console.WriteLine(new string('-', 80));

            var g2 =
                Any.OfType<string>()
                    .Where(s => !string.IsNullOrEmpty(s))
                    .SelectMany(Gen.elements, Tuple.Create);

            foreach (var sample in Gen.sample(10, 10, g2)) Console.WriteLine(sample);
        }
    }
}
