# pure-random-pcg

Implementation of the [RXS-M-XS](https://en.wikipedia.org/wiki/Permuted_congruential_generator#Variants) variant of the [permuted-congruential-generator](https://en.wikipedia.org/wiki/Permuted_congruential_generator) suite. RXS-M-XS was chosen for best browser performance.

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
  print i
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
      dirs = list choose
  print $ take 10 dirs
```

Knuth Fisher-Yates shuffle.

```haskell
main = do
  seed <- newSeed
  print $ shuffle [1..10]
```

### Thanks

This library was inspired by [Max Goldstein's](https://github.com/mgold) [elm-random-pcg](https://github.com/mgold/elm-random-pcg).
