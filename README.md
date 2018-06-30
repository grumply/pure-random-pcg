# pure-random-pcg

Implementation of the [RXS-M-XS](https://en.wikipedia.org/wiki/Permuted_congruential_generator#Variants) variant of the [permuted-congruential-generator](https://en.wikipedia.org/wiki/Permuted_congruential_generator) suite. RXS-M-XS was chosen for best browser performance.

On a 2012 Intel i7-3770 @ 3.4GHz, this implementation achieves throughputs of +60Gb/s (64-bit ints); 1,000,000,000 per second.

On the same machine in Chrome v67, this implementation achieves thoughputs of .6Gb/s (32-bit ints); 20,000,000 per second.

Be sure not to draw more than 2^32 variates from a single `Seed` on GHCJS or 32-bit GHC. If you need that many random values, use `independentSeed` to generate more `Seed`s.

On 64-bit GHC, the period for this variant of pcg is 2^64, which you'd be unlikely to exhaust.

## Features

`pure-random-pcg` comes with a convenient set of combinators.

### Generators 

Simple pure generator usage.

```haskell
main = do
  seed0 <- newSeed
  let (seed1,i)  = step int seed0
      (seed2,d)  = step double seed1
      (seed3,b)  = step bool seed2
      (seed4,i') = step (intR 0 10) seed3
      (_,d')     = step (doubleR 1.5 2.5) seed4
  print (i,d,b,i',d')
```

### Composability

`Generator` has an instance of `Functor`, `Applicative`, and `Monad`.

```haskell
data C = Double :+ Double

randomC :: Generator C
randomC = pure (:+) <*> double <*> double

randomC' :: Generator C
randomC' = do
  d1 <- double
  d2 <- double
  return (d1 :+ d2)
```

### Reproducibility with fast-forward and rewind 

`PCG` includes the interesting property that seeds can be very quickly walked forwards or backwards via `advance` and `retract`.

```retract n . advance n == id```

```haskell
main = do
  seed <- newSeed
  let seed' = advance 1000 seed
      seed'' = retract 1000 seed'
  print (seed == seed'') -- True
```

### Simple Helpers

Sample from a distribution uniformly at random.

```haskell
names = sample ["Alice","Bob","Charlie","Dan","Elaine"]

main = do
  seed <- newSeed
  print (step names seed)
```

Generate arbitrary bounded enumerables.

```haskell
data Dir = North | South | East | West deriving (Bounded,Enum,Show)

main = do
  seed <- newSeed
  let dirs :: [Dir]
      dirs = list choose seed
  print $ take 10 dirs
```

Knuth Fisher-Yates shuffle.

```haskell
main = do
  seed <- newSeed
  print $ shuffle [1..10] seed
```

### Thanks

This library was inspired by [Max Goldstein's](https://github.com/mgold) [elm-random-pcg](https://github.com/mgold/elm-random-pcg).
