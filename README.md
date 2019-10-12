# pure-random-pcg

Implementation of the [RXS-M-XS](https://en.wikipedia.org/wiki/Permuted_congruential_generator#Variants) variant of the [permuted-congruential-generator](https://en.wikipedia.org/wiki/Permuted_congruential_generator) suite. RXS-M-XS was chosen for best browser performance.

## Features

`pure-random-pcg` comes with a convenient set of combinators.

### Generators 

Simple generators.

```haskell
newtype Generator a = Generator { generate :: Seed -> (Seed,a) }
```

A variate type-class for uniform bounded and unbounded random variate generation.

```haskell
class Variate a where
  uniform :: Generator a
  uniformR :: a -> a -> Generator a
```

### Applicative and Monadic generation

```haskell
data C = Double :+ Double

randomC :: Generator C
randomC = pure (:+) <*> uniform <*> uniform

randomC' :: Generator C
randomC' = do
  d1 <- uniformR 0 255
  d2 <- uniformR 0 255
  return (d1 :+ d2)
```

### Num instance

```haskell
inc :: Num a => Generator a -> Generator a
inc = (1 +)
```

### Reproducibility

`PCG` includes the ability to very quickly walk a `Seed` forwards or backwards via `advance` and `retract`.

```retract n . advance n == id```

```haskell
main = do
  seed0 <- newSeed
  let seed1 = advance 1000 seed0
      seed2 = retract 1000 seed1
  print (seed0 == seed2) -- True
```

```haskell
main = do
  seed0 <- newSeed
  let (seed1,_) = generate uniform seed0
      seed2 = retract 1 seed1
  print (seed0 == seed2) -- True
```

### Sampling

Sample from a distribution uniformly at random.

```haskell
names = sample ["Alice","Bob","Charlie","Dan","Elaine"]

main = do
  seed <- newSeed
  print (generate names seed)
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

### Shuffling

Knuth Fisher-Yates shuffle.

```haskell
main = do
  seed <- newSeed
  print $ shuffle [1..10] seed
```

### Important Notes

On GHCJS and 32-bit GHC, this variant of pcg has a period of 2^32, meaning the pattern of random numbers will repeat after using the same `Seed` 2^32 times. If you need that many random values, use `independentSeed` to generate more seeds.

On 64-bit GHC, the period for this variant of pcg is 2^64, which you'd be unlikely to exhaust.

Keep in mind that pcg is **not** cryptographically secure.

### Performance

On a 2012 Intel i7-3770 @ 3.4GHz, this implementation achieves throughputs of +60Gb/s (64-bit ints); 1,000,000,000 per second.

On the same machine in Chrome v67, this implementation achieves thoughputs of 1Gb/s (32-bit ints); 33,000,000 per second.

Note that RXS-M-XS has a much smaller period than MWC8222 or Mersenne Twister, but is extremely performant. The major benefit of this implementation is the pure implementation that is compatible with GHCJS.

### Thanks

This library was inspired by [Max Goldstein's](https://github.com/mgold) [elm-random-pcg](https://github.com/mgold/elm-random-pcg).

Much of the Distribution and range generation code comes from [Bryan O'Sullivan's](https://github.com/bos) [mwc-random](https://github.com/bos/mwc-random).

