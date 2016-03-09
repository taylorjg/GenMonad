
## Introduction

This repo contains a simple example of using the Gen monad in a few different property-based testing tool implementations:  

* [FsCheck](https://github.com/fsharp/FsCheck)
    * Gen&lt;T&gt; (C#)
    * Gen&lt;'a&gt; (F#)
* [ScalaCheck](http://scalacheck.org/)
    * Gen&#91;+T&#93; (Scala)
* [QuickCheck](https://hackage.haskell.org/package/QuickCheck)
    * Gen a (Haskell)

The example involves a generator of a tuple where the first item is a string and the second item is a character from within the string.

## Screenshot

Here is a screenshot of the Haskell version of the code using do notation:

![Screenshot](https://raw.github.com/taylorjg/GenMonad/master/Images/Haskell_GenMonad1_Screenshot.png)

## C# / FsCheck 1.x

### Query expressions

Class <code>FsCheck.Fluent.GeneratorExtensions</code> contains the following extension methods that allow the <code>Gen&lt;T&gt;</code> type to be used in C# Query Expressions:

```C#
public static FsCheck.Gen<b> Select<a, b>(this FsCheck.Gen<a> g, System.Func<a,b> selector)
public static FsCheck.Gen<b> SelectMany<a, b>(this FsCheck.Gen<a> source, System.Func<a,Gen<b>> f)
public static FsCheck.Gen<a> Where<a>(this FsCheck.Gen<a> g, System.Func<a,bool> predicate)
```

References:

* [Monadic comprehension syntax in C# (SO)](http://stackoverflow.com/questions/19709899/monadic-comprehension-syntax-in-c-sharp)
* [C# Language Specification](http://www.microsoft.com/en-us/download/details.aspx?id=7029)

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

### Direct calls to methods in FsCheck.Fluent.GeneratorExtensions

It is also possible to call <code>FsCheck.Fluent.GeneratorExtensions</code>'s <code>SelectMany</code>, <code>Select</code>
and <code>Where</code> methods directly instead of using query expressions.

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

## C# / FsCheck 2.x

The section shows the C# code that was has been updated to work with FsCheck 2.x.

### Query expressions

```C#
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
```

### Direct calls to methods in FsCheck.GenExtensions

```C#
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
```

## F# / FsCheck

### Computation expressions

As mentioned above, FsCheck's <code>gen</code> object is a singleton instance of <code>GenBuilder</code> intended for use with F#'s computation expressions.
Effectively, <code>let!</code> expressions are translated into calls to <code>gen.Bind</code> and <code>return</code> expressions are translated
into calls to <code>gen.Return</code>.

Here we examine the types of <code>gen.Bind</code> and <code>gen.Return</code> in F# Interactive:    

```F#
> FsCheck.GenBuilder.gen.Bind;;
val it : (Gen<'a> * ('a -> Gen<'b>) -> Gen<'b>) = ...
> FsCheck.GenBuilder.gen.Return;;
val it : ('a -> Gen<'a>) = ...
```

Note how similar the signature of <code>FsCheck.GenBuilder.gen.Bind</code> is to the signature of <code>FsCheck.Fluent.GeneratorExtensions.SelectMany</code>.

References:

* [Syntax Matters: Writing abstract computations in F#](http://tomasp.net/academic/papers/computation-zoo/syntax-matters.pdf)

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

In ScalaCheck, the <code>Gen[+T]</code> trait has <code>flatMap</code> and <code>map</code> methods.
Scala's <code>for</code> expressions are translated into calls to <code>flatMap</code> and <code>map</code>.

```Scala
sealed trait Gen[+T] {
  def flatMap[U](f: T => Gen[U]): Gen[U]
  def map[U](f: T => U): Gen[U]
}
```

References:

* [How does yield work?](http://docs.scala-lang.org/tutorials/FAQ/yield.html)

### For expressions

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

### Direct calls to flatMap and map

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

In QuickCheck, Gen is an instance of the Monad type class.

```Haskell
ghci> :info Test.QuickCheck.Gen
...
instance Monad Gen -- Defined in `Test.QuickCheck.Gen'
...
ghci> :info Monad
class Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  ...
  return :: a -> m a
  ...
```

Haskell's <code>do</code> notation is translated into calls to bind (<code>&gt;&gt;=</code>) and <code>return</code>.

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

### Direct calls to >>= and fmap

```Haskell
import Test.QuickCheck

main :: IO ()
main =
    do
        let g = (arbitrary :: Gen String) `suchThat` (not . null) >>= \s ->
            (\c -> (s, c)) `fmap` (elements s)
        sample g
```
