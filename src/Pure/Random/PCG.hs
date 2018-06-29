{-# LANGUAGE ViewPatterns, BangPatterns, CPP, ScopedTypeVariables #-}
module Pure.Random.PCG (module Pure.Random.PCG, newSeed, initialSeed, System.Random.RandomGen(..), System.Random.Random(..)) where

import Pure.Random.PCG.Internal

import Control.Monad
import Control.Arrow ((&&&))
import Data.Bits ((.|.),(.&.),xor)
import Data.Foldable as F
import Data.List
import Data.Int

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import qualified System.Random

-- Inspired by Max Goldstein's elm-random-pcg library: https://github.com/mgold/elm-random-pcg
-- On GHC and 64-bit word size, Pure.Random.PCG.Internal uses the 64-bit RXS M XS pcg variant.
-- On GHCJS or GHC and 32-bit word size, Pure.Random.PCG.Internal uses the 32-bit RXS M XS pcg variant.
-- On GHC, in trivial cases, this implementation runs on the order of 60 Gb/s for Ints (64-bit) and 6Gb/s for Doubles.
--
-- For Doubles, the code produced by GHCJS is quite a bit slower than the browser-based Math.random().
-- For non-Doubles, especially Ints, the code produced by GHCJS is competetive with or better than the 
-- browser-based Math.random(). 
--
-- Where this library excels is in easily creating and composing reproducible random streams.
--
-- For best performance, use the primitive Generators (int,intR,double,doublrR,bool,oneIn)
-- along with (list,sample,sampleVector) rather than the System.Random.Random typeclass methods.
--
-- To construct complex values, use the Applicative instance:
-- 
-- > data C = Int :+ Int
-- >
-- > randomC :: Generator C
-- > randomC = pure (:+) <*> int <*> int
--
-- 

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

{-# INLINE doubleR #-}
doubleR :: Double -> Double -> Generator Double
doubleR lo hi
  | hi < lo   = doubleR hi lo
  | lo == hi  = pure lo
  | otherwise = Generator go
    where
      {-# INLINE go #-}
      go seed =
        let 
            !seed' = pcg_next seed
            !x = fromIntegral (pcg_peel seed) :: Int32
            !hhi = 0.5 * hi
            !hlo = 0.5 * lo
            !scaled_x = (hlo + hhi) + ((hhi - hlo) / halfInt32Count) * fromIntegral x
        in 
            ( seed', scaled_x )


{-# INLINE double #-}
double :: Generator Double
double = doubleR 0.0 1.0

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

{-# INLINE sampleVector #-}
sampleVector :: V.Vector a -> Generator (Maybe a)
sampleVector v =
    let !l = V.length v
    in if l == 0 then pure Nothing else Generator (go l)
  where
    {-# INLINE go #-}
    go len seed =
        let (!seed',!l) = step (boundedRand len) seed
            x = V.unsafeIndex v l
        in (seed',Just x)

{-# INLINE independentSeed #-}
independentSeed :: Generator Seed
independentSeed = Generator go 
  where
    {-# INLINE go #-}
    go seed0 = 
        let !gen = intR 0 maxBound
            (!seed1,(!state,!b,!c)) = step (pure (,,) <*> gen <*> gen <*> gen) seed0
            !incr = (b `xor` c) .|. 1
            !seed' = pcg_next (Seed state incr)
        in (seed',seed1)

{-# INLINE choose #-}
choose :: forall a. (Bounded a,Enum a) => Generator a
choose = pure toEnum <*> intR 0 (fromEnum (maxBound :: a))

{-# INLINE list #-}
list :: Generator a -> Seed -> [a]
list gen = unfoldr (\seed -> let (!seed',!r) = step gen seed in Just (r,seed'))

{-# INLINE advance #-}
advance :: Int -> Seed -> Seed
advance = flip pcg_advance

{-# INLINE retract #-}
-- retract n . advance n == id
retract :: Int -> Seed -> Seed
retract steps = flip pcg_advance (negate steps)

{-# INLINE shuffle #-}
shuffle :: [a] -> Seed -> [a]
shuffle as = V.toList . shuffleVector (V.fromList as)

{-# INLINE shuffleVector #-}
shuffleVector :: V.Vector a -> Seed -> V.Vector a
shuffleVector v0 seed = V.modify (\v -> go v seed (MV.length v - 1)) v0
  where
    {-# INLINE go #-}
    go v = go' 
      where
        {-# INLINE go' #-}
        go' !seed !i 
          | i <= 0    = return ()
          | otherwise = do
            let (seed',j) = step (intR 0 (i + 1)) seed
            MV.unsafeSwap v i j
            go' seed' (i - 1)

instance System.Random.RandomGen Seed where
    {-# INLINE next #-}
    next seed = 
        let (!seed',!i) = step int seed
        in (i,seed')
    {-# INLINE split #-}
    split seed =
        let (!seed',!randSeed) = step independentSeed seed
        in (randSeed,seed')
