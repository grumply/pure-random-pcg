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
  d1 <- uniform
  d2 <- uniform
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

Keep in mind that pcg is **not** cryptographically secure.

### Performance

On an i9-9880H, this implementation achieves throughput of 2 bytes per cycle (64Gb/s for 64-bit types).

On the same machine in Chrome v93, this implementation achieves thoughput of 1Gb/s for 32-bit types.

Note that RXS-M-XS has a much smaller period than MWC8222 or Mersenne Twister, but is extremely performant. The major benefit of this implementation is the pure implementation that is compatible with GHCJS.

### Thanks

This library was inspired by [Max Goldstein's](https://github.com/mgold) [elm-random-pcg](https://github.com/mgold/elm-random-pcg).

Much of the Distribution and range generation code comes from [Bryan O'Sullivan's](https://github.com/bos) [mwc-random](https://github.com/bos/mwc-random).

