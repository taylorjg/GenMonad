
## C# / FsCheck

### Query expression
 
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
            var g =
                from s in Arb.generate<string>()
                where !string.IsNullOrEmpty(s)
                from c in Gen.elements(s)
                select Tuple.Create(s, c);

            foreach (var sample in Gen.sample(10, 10, g)) Console.WriteLine(sample);
        }
    }
}
```

### Direct calls to GeneratorExtensions.SelectMany and GeneratorExtensions.Select 

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

### Direct calls to GenBuilder.gen.Bind and GenBuilder.gen.Return 

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

## F# / FsCheck

### Computation expression

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

## Scala / ScalaCheck

### For expression

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

### Direct calls to Gen.flatMap and Gen.map

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

## Haskell / QuickCheck

### Do notation

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

### Direct calls to >>= and return
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
