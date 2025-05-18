# TEA

Exercise consisting of implementing The Elm Architecture (TEA), but in Haskell.

The [Gren](https://gren-lang.org/) programming language is already using that
pattern to develop backend apps, food for thought...

## NOTE

Calling into `IO` is pretty much impossible with this design (simulating
Elm/Gren workflows), see the cabal file and the `NoImplicitPrelude` option.

So, with these constraint, you develop your app as a cabal _library_, and you
boot up your app via a "runner".

The runner itself can call into `IO`, but only to actually boot up the program
definition specified by the `TEA` module.

## IDEAS/TODO

- prevent using `String`, preferring `Text` instead
  - ban `show`?
  - or just let the "library app" author call into the `text` package?
- expand the `TEA.Prelude` to more useful types, as it's currently very
  restricted
