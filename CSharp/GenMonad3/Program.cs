using System;
using FsCheck;
using FsCheck.Fluent;
using Microsoft.FSharp.Core;

namespace GenMonad3
{
    internal static class Program
    {
        private static void Main()
        {
            var gen = GenBuilder.gen;

            var g =
                gen.Bind(
                    Any.OfType<string>().Where(s => !string.IsNullOrEmpty(s)),
                    FSharpFunc<string, Gen<Tuple<string, char>>>.FromConverter(s =>
                        gen.Bind(
                            Gen.elements(s),
                            FSharpFunc<char, Gen<Tuple<string, char>>>.FromConverter(c =>
                                gen.Return(Tuple.Create(s, c))))));

            foreach (var sample in Gen.sample(10, 10, g)) Console.WriteLine(sample);
        }
    }
}
