{-# LANGUAGE ViewPatterns, BangPatterns #-}
module Pure.Random.PCG (module Pure.Random.PCG, newSeed, initialSeed, System.Random.RandomGen(..), System.Random.Random(..)) where

import Pure.Random.PCG.Internal

import Control.Monad
import Control.Arrow ((&&&))
import Data.Bits ((.|.),xor)
import Data.Foldable as F
import Data.List

import qualified System.Random

-- Modeled after Max Goldstein's elm-random-pcg library: https://github.com/mgold/elm-random-pcg
-- This module is meant for GHCJS and will not generate sufficiently random values when `sizeOf (undefined :: Int) == 64`.

newtype Generator a = Generator { generate :: Seed -> (Seed,a) }
instance Functor Generator where
    {-# INLINE fmap #-}
    fmap f (Generator g) = Generator (fmap (\(seed,a) -> let !b = f a in (seed,b)) g)
instance Applicative Generator where
    {-# INLINE pure #-}
    pure a = Generator (\seed -> (seed,a)) -- useful for (a -> b), bad for Ints....
    {-# INLINE (<*>) #-}
    (<*>) (Generator gfs) (Generator gas) = Generator $ \seed0 ->
        let
            (!seed1,!f) = gfs seed0
            (!seed2,!a) = gas seed1
            !b = f a
        in
            (seed2,b)
instance Monad Generator where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    ga >>= agb = Generator $ \seed -> 
        let (!seed',!a) = step ga seed
            !gb = agb a
        in generate gb seed'

{-# INLINE step #-}
step :: Generator a -> Seed -> (Seed,a)
step (Generator gen) = gen

{-# INLINE boundedRand #-}
boundedRand :: Int -> Generator Int
boundedRand bound = Generator $
    let !threshold = negate bound `rem` bound
    in go threshold
  where
    go threshold = go'
      where
        go' (generate randomint -> (!seed,!r)) 
          | r >= threshold = 
            let !br = r `rem` bound
            in (seed,br)
          | otherwise = go' seed

{-# INLINE int #-}
int :: Int -> Int -> Generator Int
int lo hi
  | lo < hi = let !range = hi - lo + 1
              in pure (+ lo) <*> boundedRand range
  | hi < lo = let !range = lo - hi + 1
              in pure (+ lo) <*> boundedRand range
  | otherwise = pure lo

{-# INLINE bool #-}
bool :: Generator Bool
bool = pure (== 1) <*> boundedRand 1

{-# INLINE oneIn #-}
oneIn :: Int -> Generator Bool
oneIn n = pure (== 1) <*> boundedRand n

{-# INLINE sample #-}
sample :: Foldable f => f a -> Generator (Maybe a)
sample xs = 
    let !l = F.length xs
    in if l == 0 then pure Nothing else Generator (go l)
  where
    {-# INLINE go #-}
    go len seed = 
        let (!seed',!l) = step (boundedRand len) seed
            !x = F.foldr go' (const Nothing) xs l
        in (seed',x)

    {-# INLINE go' #-}
    go' x continue !n
      | n == 0 = Just x
      | otherwise = continue (n - 1)

{-# INLINE independentSeed #-}
independentSeed :: Generator Seed
independentSeed = Generator go 
  where
    {-# INLINE go #-}
    go seed0 = 
        let !gen = int 0 maxBound
            (!seed1,(!state,!b,!c)) = step (pure (,,) <*> gen <*> gen <*> gen) seed0
            !incr = (b `xor` c) .|. 1
            !seed' = pcg_next (Seed state incr)
        in (seed',seed1)

{-# INLINE randomint #-}
randomint :: Generator Int
randomint = Generator (pcg_step &&& pcg_peel)

{-# INLINE randomints #-}
randomints :: Seed -> [Int]
randomints = unfoldr (\seed -> let (!seed',!r) = step randomint seed in Just (r,seed'))

{-# INLINE ints #-}
ints :: Int -> Int -> Seed -> [Int]
ints lo hi = 
    let !gen = int lo hi
    in unfoldr (\seed -> let (!seed',!r) = step gen seed in Just (r,seed')) 

instance System.Random.RandomGen Seed where
    {-# INLINE next #-}
    next seed = 
        let (!seed',!i) = step randomint seed
        in (i,seed')
    {-# INLINE split #-}
    split seed =
        let (!seed',!randSeed) = step independentSeed seed
        in (randSeed,seed')
