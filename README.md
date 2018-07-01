# pure-random-pcg

Implementation of the [RXS-M-XS](https://en.wikipedia.org/wiki/Permuted_congruential_generator#Variants) variant of the [permuted-congruential-generator](https://en.wikipedia.org/wiki/Permuted_congruential_generator) suite. RXS-M-XS was chosen for best browser performance.

On a 2012 Intel i7-3770 @ 3.4GHz, this implementation achieves throughputs of +60Gb/s (64-bit ints); 1,000,000,000 per second.

On the same machine in Chrome v67, this implementation achieves thoughputs of 1Gb/s (32-bit ints); 33,000,000 per second.

Be sure not to draw more than 2^32 variates from a single `Seed` on GHCJS or 32-bit GHC. If you need that many random values, use `independentSeed` to generate more `Seed`s.

On 64-bit GHC, the period for this variant of pcg is 2^64, which you'd be unlikely to exhaust.

RXS-M-XS has a much smaller period than MWC8222 or Mersenne Twister, but is 3-5x faster than SFMT(SIMD Fast Mersenne Twister), and 2-7x faster than MWC8222, and nearly 1000x faster than `random`'s System.Random. 

The implementation of SFMT doesn't naturally support bounded variate generation. All testing done using similar loops compiled with `-fllvm -O2`. SFMT was compiled with SSE2/SIMD enabled.

| Algorithm    | Int   | Int8  | Int16 | Int32 | Int64 |
| ------------ | ----- | ----- | ----- | ----- | ----- | 
| RXS-M-XS pcg | 1ns   | 1ns   | 1ns   | 1ns   | 1ns   | 
| SFMT         | 3.3ns | 3.3ns | 3.3ns | 3.3ns | 3.3ns |
| MWC8222      | 7ns   | 3.7ns | 3.7ns | 3.7ns | 7ns   |


| Algorithm    | Bounded Int* | Bounded Int8 | Bounded Int16 | Bounded Int32 | Bounded Int64* |
| ------------ | ------------ | ------------ | ------------- | ------------- | -------------- |
| RXS-M-XS pcg | 12ns/1ns     | 1ns          | 1ns           | 1ns           | 5ns/1ns        |
| MWC8222      | 22ns/7.4ns   | 8.5ns        | 7.7ns         | 7.2ns         | 14.9ns/7.4ns   |

| Algorithm    | Word  | Word8 | Word16 | Word32 | Word64 |
| ------------ | ----- | ----- | ------ | ------ | ------ |
| RXS-M-XS pcg | 1ns   | 1ns   | 1ns    | 1ns    | 1ns    |
| SFMT         | 3.3ns | 3.3ns | 3.3ns  | 3.5ns  | 3.5ns  |
| MWC8222      | 7ns   | 3.7ns | 3.7ns  | 3.7ns  | 7ns    |

| Algorithm    | Bounded Word* | Bounded Word8 | Bounded Word16 | Bounded Word32 | Bounded Word64* |
| ------------ | ------------- | ------------- | -------------- | -------------- | --------------- |
| RXS-M-XS     | 5.2ns/1ns     | 1ns           | 1ns            | 1ns            | 5.2ns/1ns       |
| MWC8222      | 17.3ns/7.8ns  | 8.5ns         | 7.7ns          | 7.2n           | 17.3ns/7.3ns    |

| Algorithm    | Double | Float | 
| ------------ | ------ | ----- | 
| RXS-M-XS pcg | 1ns    | 1ns   |
| SFMT         | 4.8ns  | -     |
| MWC8222      | 7.5ns  | 3.7ns |

| Algorithm    | Bounded Double | Bounded Float |
| ------------ | -------------- | ------------- |
| RXS-M-XS pcg | 1ns            | 1ns           |
| MWC8222      | 7.6ns          | 3.8ns         |

* Bounded Ints/Int64/Word/Word64 in (lo,hi) can be more efficiently produced if (hi - lo < maxBound :: Int32)

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
