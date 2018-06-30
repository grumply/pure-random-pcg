{-# LANGUAGE ViewPatterns, BangPatterns, CPP, ScopedTypeVariables #-}
module Pure.Random.PCG (module Pure.Random.PCG, newSeed, initialSeed, System.Random.RandomGen(..), System.Random.Random(..)) where

import Pure.Random.PCG.Internal

import Control.Monad
import Control.Arrow ((&&&))
import Data.Bits ((.|.),(.&.),xor)
import Data.List
import Data.Int

import qualified System.Random

-- Inspired by Max Goldstein's elm-random-pcg library: https://github.com/mgold/elm-random-pcg
-- On GHC and 64-bit word size, Pure.Random.PCG.Internal uses the 64-bit RXS M XS pcg variant.
-- On GHCJS or GHC and 32-bit word size, Pure.Random.PCG.Internal uses the 32-bit RXS M XS pcg variant.
--
-- For Doubles, the code produced by GHCJS is quite a bit slower than the browser-based Math.random().
-- For non-Doubles, especially Ints, the code produced by GHCJS is competetive with or better than the 
-- browser-based Math.random(). 
--
-- For best performance, use the primitive Generators (int,intR,double,doublrR,bool,oneIn)
-- along with (list,sample,sampleVector) rather than the System.Random.Random typeclass methods.

newtype Generator a = Generator { generate :: Seed -> (Seed,a) }
instance Functor Generator where
    {-# INLINE fmap #-}
    fmap f (Generator g) = Generator (fmap (\(seed,a) -> let !b = f a in (seed,b)) g)
instance Applicative Generator where
    {-# INLINE pure #-}
    pure a = Generator (\seed -> (seed,a))
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
        let (!seed',!a) = generate ga seed
            !gb = agb a
        in generate gb seed'

{-# INLINE boundedRand #-}
boundedRand :: Int -> Generator Int
boundedRand bound = Generator $
    let !threshold = negate bound `rem` bound
    in go threshold
  where
    {-# INLINE go #-}
    go threshold = go'
      where
        {-# INLINE go' #-}
        go' (generate int -> (!seed,!r)) 
          | r >= threshold = 
            let !br = r `rem` bound
            in (seed,br)
          | otherwise = go' seed

{-# INLINE int #-}
int :: Generator Int
int = Generator (pcg_next &&& pcg_peel)

{-# INLINE intR #-}
intR :: Int -> Int -> Generator Int
intR lo hi
  | hi < lo   = intR hi lo 
  | hi == lo  = pure lo
  | otherwise = 
    let !range = hi - lo + 1
    in pure (+ lo) <*> boundedRand range

{-# INLINE independentSeed #-}
independentSeed :: Generator Seed
independentSeed = Generator go 
  where
    {-# INLINE go #-}
    go seed0 = 
        let !gen = intR 0 maxBound
            (!seed1,(!state,!b,!c)) = generate (pure (,,) <*> gen <*> gen <*> gen) seed0
            !incr = (b `xor` c) .|. 1
            !seed' = pcg_next (Seed state incr)
        in (seed',seed1)

{-# INLINE list #-}
list :: Generator a -> Seed -> [a]
list gen = unfoldr (\seed -> let (!seed',!r) = generate gen seed in Just (r,seed'))

{-# INLINE advance #-}
advance :: Int -> Seed -> Seed
advance = flip pcg_advance

{-# INLINE retract #-}
-- retract n . advance n == id
retract :: Int -> Seed -> Seed
retract steps = flip pcg_advance (negate steps)

instance System.Random.RandomGen Seed where
    {-# INLINE next #-}
    next seed = 
        let (!seed',!i) = generate int seed
        in (i,seed')
    {-# INLINE split #-}
    split seed =
        let (!seed',!randSeed) = generate independentSeed seed
        in (randSeed,seed')
