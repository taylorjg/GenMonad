using System;
using FsCheck;
using FsCheck.Fluent;

namespace GenMonad2
{
    internal class Program
    {
        private static void Main()
        {
            var g =
                Any.OfType<string>()
                    .Where(s => !string.IsNullOrEmpty(s))
                    .SelectMany(s => Gen.elements(s)
                        .Select(c => Tuple.Create(s, c)));

            foreach (var sample in Gen.sample(10, 10, g)) Console.WriteLine(sample);
        }
    }
}
