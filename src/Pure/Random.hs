{-# LANGUAGE CPP, FlexibleContexts, TypeFamilies, ScopedTypeVariables, BangPatterns, MagicHash, UnboxedTuples, TypeApplications, ViewPatterns #-}
module Pure.Random (module Pure.Random, module Pure.Random.PCG, System.Random.RandomGen(..), System.Random.Random(..)) where

import Control.Monad.ST
import Data.Bits
import Data.Foldable as F
import Data.Int
import Data.Proxy
import Data.Word

import Pure.Random.PCG
import Pure.Random.PCG.Internal

import Control.Monad.Primitive

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U

import qualified System.Random

import GHC.Prim
import GHC.Word

#ifndef __GHCJS__
#import "MachDeps.h"
#endif

instance System.Random.RandomGen Seed where
    {-# INLINE next #-}
    next seed = 
        let (!seed',!i) = generate int seed
        in (i,seed')
    {-# INLINE split #-}
    split seed =
        let (!seed',!randSeed) = generate independentSeed seed
        in (randSeed,seed')

-- | Variate taken from Bryan O'Sullivan's mwc-random with changes made for
-- uniformR when WORD_SIZE_IN_BITS == 32, for performance. 
--
-- Some concessions were made to keep bounded generation non-conditional and efficient.
--
-- All bounded non-fractional generators produce in the range (lo,hi].
-- All bounded fractional generators produce in the range
-- Unbounded non-fractional generators produce in the range [minBound..maxBound].
-- Unbounded fractional generators produce in the range (0,1].
-- 
class Variate a where
    uniform :: Generator a
    uniformR :: a -> a -> Generator a

{-# RULES
    "uniformR x x" forall x. uniformR x x = pure x
#-}

instance Variate Int8 where
    -- | Produces in the range [minBound..maxBound]
    uniform = pure fromIntegral <*> int
    {-# INLINE uniform #-}
    -- | Produces in the range [lo + 1..hi]
    uniformR a b = fmap (a +) (uniformRange_internal (b - a))
    {-# INLINE uniformR #-}

instance Variate Int16 where
    -- | Produces in the range [minBound..maxBound]
    uniform = pure fromIntegral <*> int
    {-# INLINE uniform #-}
    -- | Produces in the range [lo + 1..hi]
    uniformR a b = fmap (a +) (uniformRange_internal (b - a))
    {-# INLINE uniformR #-}

instance Variate Int32 where
    -- | Produces in the range [minBound..maxBound]
    uniform = pure fromIntegral <*> int
    {-# INLINE uniform #-}
    -- | Produces in the range [lo + 1..hi]
    uniformR a b = fmap (a +) (uniformRange_internal (b - a))
    {-# INLINE uniformR #-}

instance Variate Int64 where
    -- | Produces in the range [minBound..maxBound]
#if (defined __GHCJS__ || WORD_SIZE_IN_BITS == 32)
    uniform = pure wordsTo64Bit <*> uniform <*> uniform
#else
    uniform = fmap fromIntegral int
#endif
    {-# INLINE uniform #-}
    -- | Produces in the range [lo + 1..hi]
    uniformR a b = fmap (a +) (uniformRange_internal (b - a))
    {-# INLINE uniformR #-}

instance Variate Int where
    -- | Produces in the range [minBound..maxBound]
    uniform = int
    {-# INLINE uniform #-}
    -- | Produces in the range [lo + 1..hi]
    uniformR a b = fmap (a +) (uniformRange_internal (b - a))
    {-# INLINE uniformR #-}

instance Variate Word8 where
    -- | Produces in the range [minBound..maxBound]
    uniform = pure fromIntegral <*> int 
    {-# INLINE uniform #-}
    -- | Produces in the range [lo + 1..hi]
    uniformR a b = fmap (a +) (uniformRange_internal (b - a))
    {-# INLINE uniformR #-}

instance Variate Word16 where
    -- | Produces in the range [minBound..maxBound]
    uniform = pure fromIntegral <*> int 
    {-# INLINE uniform #-}
    -- | Produces in the range [lo + 1..hi]
    uniformR a b = fmap (a +) (uniformRange_internal (b - a))
    {-# INLINE uniformR #-}

instance Variate Word32 where
    -- | Produces in the range [minBound..maxBound]
    uniform = pure fromIntegral <*> int 
    {-# INLINE uniform #-}
    -- | Produces in the range [lo + 1..hi]
    uniformR a b = fmap (a +) (uniformRange_internal (b - a))
    {-# INLINE uniformR #-}

instance Variate Word64 where
    -- | Produces in the range [minBound..maxBound]
#if (defined __GHCJS__ || WORD_SIZE_IN_BITS == 32)
    uniform = pure wordsTo64Bit <*> uniform <*> uniform
#else
    uniform = pure fromIntegral <*> int
#endif
    {-# INLINE uniform #-}
    -- | Produces in the range [lo + 1..hi]
    uniformR a b = fmap (a +) (uniformRange_internal (b - a))
    {-# INLINE uniformR #-}

instance Variate Word where
    -- | Produces in the range [minBound..maxBound]
    uniform = pure fromIntegral <*> int
    {-# INLINE uniform #-}
    -- | Produces in the range [lo + 1..hi]
    uniformR a b = fmap (a +) (uniformRange_internal (b - a))
    {-# INLINE uniformR #-}

instance Variate Bool where
    uniform = pure wordToBool <*> uniform
    {-# INLINE uniform #-}
    uniformR a b 
      | a < b = pure a
      | otherwise = uniform
    {-# INLINE uniformR #-}

instance Variate Float where
    -- | Produces in the range [2**(-33)..1]
    uniform = pure wordToFloat <*> uniform
    {-# INLINE uniform #-}
    -- | Produces in the range [lo..hi]
    uniformR a b = pure (\w -> a + (b - a) * wordToFloat w) <*> uniform
    {-# INLINE uniformR #-}

instance Variate Double where
#if ( defined __GHCJS__ || WORD_SIZE_IN_BITS == 32 )
    -- | Produces in the range [2**(-53)..1]
    uniform = pure wordsToDouble <*> uniform <*> uniform
    {-# INLINE uniform #-}
    -- | Produces in the range [lo..hi]
    uniformR a b = pure (\w1 w2 -> a + (b - a) * wordsToDouble w1 w2) <*> uniform <*> uniform
    {-# INLINE uniformR #-}
#else
    -- | Produces in the range [2**(-53)..1]
    uniform = pure word64ToDouble <*> uniform
    {-# INLINE uniform #-}
    -- | Produces in the range [lo..hi]
    uniformR a b = fmap (\w -> a + (b - a) * wordsToDouble (fromIntegral $ w `unsafeShiftR` 32) (fromIntegral w)) (uniform @Word64)
    {-# INLINE uniformR #-}
#endif

{-# INLINE bool #-}
bool :: Generator Bool
bool = oneIn 2

{-# INLINE oneIn #-}
oneIn :: Int -> Generator Bool
oneIn n = pure (== 1) <*> uniformR 0 n

{-# INLINE sample #-}
sample :: Foldable f => f a -> Generator (Maybe a)
sample = sampleVector . V.fromList . F.toList

{-# INLINE sampleVector #-}
sampleVector :: G.Vector v a => v a -> Generator (Maybe a)
sampleVector v =
    let !l = G.length v
    in if l == 0 then pure Nothing else Generator (go l)
  where
    {-# INLINE go #-}
    go len seed =
        let (!seed',!l) = fmap (subtract 1) (generate (uniformR 0 len) seed)
            x = G.unsafeIndex v l
        in (seed',Just x)

{-# INLINE shuffle #-}
shuffle :: [a] -> Seed -> [a]
shuffle as = V.toList . shuffleVector (V.fromList as)

{-# INLINE shuffleVector #-}
shuffleVector :: G.Vector v a => v a -> Seed -> v a
shuffleVector v0 seed = G.modify (flip shuffleMVector seed) v0

{-# INLINE shuffleMVector #-}
shuffleMVector :: (PrimMonad m, GM.MVector v a) => v (PrimState m) a -> Seed -> m ()
shuffleMVector v = go (GM.length v)
  where
    {-# INLINE go #-}
    go !i !seed
      | i <= 1    = return ()
      | otherwise = do
        let destination = i - 1
            (seed',source) = fmap (subtract 1) (generate (uniformR 0 i) seed)
        GM.unsafeSwap v source destination
        go destination seed' 

{-# INLINE choose #-}
choose :: forall a. (Bounded a,Enum a) => Generator a
choose = pure toEnum <*> fmap (subtract 1) (uniformR 0 (succ (fromEnum (maxBound :: a))))

-- Based on Stephen Canon's insight: 
-- https://github.com/apple/swift/pull/39143
--
-- Generates in the range 0<..upper
--
-- This unconditional bounded range generator solves all previous performance
-- problems with bounded integral generation and makes all exported numeric 
-- generators (<= WORD_SIZE) approximately performance-equivalent. Lovely!
{-# INLINE uniformRange_internal #-}
uniformRange_internal :: Integral a => a -> Generator a
uniformRange_internal (fromIntegral -> W# upper) = do
    word1 :: Word <- uniform
    word2 :: Word <- uniform
    pure $ fromIntegral $ W#
      (case (# word1, word2 #) of
        (# W# w1, W# w2 #) -> 
          case timesWord2# upper w1 of
            (# result, fraction #) -> 
              case timesWord2# upper w2 of
                (# x, _ #) -> 
                  case plusWord2# fraction x of
                    (# _, carry #) -> 
                      plusWord# result (int2Word# (gtWord# carry (int2Word# 0#)))
      )

{-
uniformRange_internal :: forall a.  ( Integral a, Bounded a, Variate a) => a -> a -> Generator a
uniformRange_internal x1 x2 = Generator go
  where
    {-# INLINE go #-}
    go seed
        | n == 0    = generate uniform seed
        | otherwise = loop seed
        where
            (# i, j #) | x1 < x2   = (# x1, x2 #)
                       | otherwise = (# x2, x1 #)
            n = 1 + isub j i :: Word64
            buckets = maxBound `div` n
            maxN    = buckets * n
            {-# INLINE loop #-}
            loop seed = 
                let (!seed',!x) = generate uniform seed
                in if x < maxN 
                    then 
                        let !br = iadd i (fromIntegral (x `div` buckets))
                        in (seed',br)
                    else loop seed'
-}

{-# INLINE independentSeed #-}
independentSeed :: Generator Seed
independentSeed = Generator go 
  where
    {-# INLINE go #-}
    go seed0 = 
        let !gen = uniformR (-1) maxBound
            (!seed1,(!state,!b,!c)) = generate (pure (,,) <*> gen <*> gen <*> gen) seed0
            !incr = (b `xor` c) .|. 1
            !seed' = pcg_next (Seed state incr)
        in (seed',seed1)

{-# INLINE vector #-}
vector :: forall v a. (G.Vector v a) => Int -> Generator a -> Generator (v a)
vector n g = 
    -- Since the state isn't recoverable from G.unfoldrN, 
    -- we use an independent seed.
    pure (G.unfoldrN n go) <*> independentSeed
  where
    {-# INLINE go #-}
    go :: Seed -> Maybe (a,Seed)
    go seed = 
        let (seed',a) = generate g seed
        in Just (a,seed')

-- structural `pred` for Double
-- `uniform` for Double produces in the range (0,1]. 
-- To produce a Double in the interval [0,1):
--
-- > pure sub <*> uniform
--
{-# INLINE sub #-}
sub :: Double -> Double
sub = subtract (2**(-53))

-- structural `succ` for Double
{-# INLINE sup #-}
sup :: Double -> Double
sup = (+ (2**(-53)))

-- structural `pred` for Float
{-# INLINE subFloat #-}
subFloat :: Float -> Float
subFloat = subtract (2**(-33))

-- structural `succ` for Float
{-# INLINE supFloat #-}
supFloat :: Float -> Float
supFloat = (+ (2**(-33)))