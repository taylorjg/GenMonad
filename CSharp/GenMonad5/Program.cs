using System;
using FsCheck;

namespace GenMonad5
{
    internal static class Program
    {
        private static void Main()
        {
            var g =
                Arb.Generate<string>()
                    .Where(s => !string.IsNullOrEmpty(s))
                    .SelectMany(Gen.Elements, Tuple.Create);

            foreach (var sample in Gen.Sample(10, 10, g)) Console.WriteLine(sample);
        }
    }
}
