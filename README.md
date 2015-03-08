
## C&#23;
 
```C#
using System;
using System.Linq;
using FsCheck;
using FsCheck.Fluent;

namespace GenMonad1
{
    internal class Program
    {
        private static void Main()
        {
            var g1 =
                from s in Arb.generate<string>()
                where !string.IsNullOrEmpty(s)
                from c in Gen.elements(s)
                select Tuple.Create(s, c);
            foreach (var sample in Gen.sample(10, 10, g1)) Console.WriteLine(sample);
        }
    }
}
```

```C#
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
```

```C#
using System;
using FsCheck;
using FsCheck.Fluent;
using Microsoft.FSharp.Core;

namespace GenMonad3
{
    internal class Program
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
```

## F&#23;

```F#
open FsCheck
open Gen
open System

[<EntryPoint>]
let main _ = 
    let g = gen {
        let! s = Arb.generate<string> |> suchThat (String.IsNullOrEmpty >> not)
        let! c = elements s
        return (s, c)
    }
    sample 50 10 g |> Seq.iter (printfn "%A")
    0
```

## Scala

```Scala
import org.scalacheck.Gen._

object GenMonad1 {
	def main(args: Array[String]) = {
		val g = for {
			s <- alphaStr suchThat { !_.isEmpty }
			c <- oneOf(s)
		} yield (s, c)
		for { _ <- 1 to 10 } g.sample.map(println)
	}
}
```

```Scala
import org.scalacheck.Gen._

object GenMonad2 {
	def main(args: Array[String]) = {
		val g = alphaStr suchThat { !_.isEmpty } flatMap (s =>
			oneOf(s) map (c =>
				(s, c)))
		for { _ <- 1 to 10 } g.sample.map(println)
	}
}
```

## Haskell

```Haskell
import Test.QuickCheck

main :: IO ()
main =
	do
		let g = do
			s <- (arbitrary :: Gen String) `suchThat` (not . null)
			c <- elements s
			return (s, c)
		sample g
```

```Haskell
import Test.QuickCheck

main :: IO ()
main =
	do
		let g = (arbitrary :: Gen String) `suchThat` (not . null) >>= \s ->
			elements s >>= \c ->
				return (s, c)
		sample g
```
