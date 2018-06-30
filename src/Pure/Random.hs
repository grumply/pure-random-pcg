{-# LANGUAGE CPP, FlexibleContexts, TypeFamilies, ScopedTypeVariables, BangPatterns, MagicHash, UnboxedTuples #-}
module Pure.Random (module Pure.Random, module Pure.Random.PCG, System.Random.RandomGen(..), System.Random.Random(..)) where

import Data.Bits
import Data.Foldable as F
import Data.Int
import Data.Proxy
import Data.Word

import Pure.Random.PCG
import Pure.Random.PCG.Internal

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import qualified System.Random

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

-- | Variate taken from Bryan O'Sullivan's mwc-random with changes made 
-- for uniformR for WORD_SIZE_IN_BITS <= 32 for performance. 
--
-- All integral types produce in the range [inclusive,inclusive], and
-- fractional types produce in the range (exclusive,inclusive].
--
-- When generating bounded 64-bit values, if your range is less than 
-- (maxBound :: Int32), generate a bounded Int32 variate in the range 
-- (0,hi - lo) and add it to lo:
-- 
-- > let rangeGen = uniformR 0 (hi - lo) :: Generator Int32
-- >     (seed',r) = generate rangeGen seed
-- > in (seed',lo + r)
--
-- Fixing 64-bit generator performance is an open problem. All other generators
-- should be reasonably fast at ~(byte-size * 8 Gb/s) while bounded 64-bit 
-- values are ~(6.4 Gb/s) on a 2012 Intel(R) Core(TM) i7-3770 CPU @ 3.40GHz.
--
-- In GHCJS, performance is on par with the browser's Math.random() except in the 
-- cases mentioned above where the same slowdown is seen for 64-bit values.
--
class Variate a where
    uniform :: Generator a
    uniformR :: a -> a -> Generator a

instance Variate Int8 where
    uniform = pure fromIntegral <*> int
    {-# INLINE uniform #-}
    uniformR a b = pure (floor :: Double -> Int8) <*> uniformR (fromIntegral a) (fromIntegral b + 1)
    {-# INLINE uniformR #-}

instance Variate Int16 where
    uniform = pure fromIntegral <*> int
    {-# INLINE uniform #-}
    uniformR a b = pure (floor :: Double -> Int16) <*> uniformR (fromIntegral a) (fromIntegral b + 1)
    {-# INLINE uniformR #-}

instance Variate Int32 where
    uniform = pure fromIntegral <*> int
    {-# INLINE uniform #-}
    uniformR a b = pure (floor :: Double -> Int32) <*> uniformR (fromIntegral a) (fromIntegral b + 1)
    {-# INLINE uniformR #-}

instance Variate Int64 where
#if (defined __GHCJS__ || WORD_SIZE_IN_BITS == 32)
    uniform = pure wordsTo64Bit <*> uniform <*> uniform
#else
    uniform = pure fromIntegral <*> int
#endif
    {-# INLINE uniform #-}
    uniformR a b = uniformRange (Proxy :: Proxy Word64) a b
    {-# INLINE uniformR #-}

instance Variate Int where
    uniform = int
    {-# INLINE uniform #-}
#if (defined __GHCJS__ || WORD_SIZE_IN_BITS == 32)
    uniformR a b = pure (floor :: Double -> Int) <*> uniformR (fromIntegral a) (fromIntegral b + 1)
#else
    uniformR a b = uniformRange (Proxy :: Proxy Word64) a b
#endif
    {-# INLINE uniformR #-}

instance Variate Word8 where
    uniform = pure fromIntegral <*> int 
    {-# INLINE uniform #-}
    uniformR a b = pure (floor :: Double -> Word8) <*> uniformR (fromIntegral a) (fromIntegral b + 1)
    {-# INLINE uniformR #-}

instance Variate Word16 where
    uniform = pure fromIntegral <*> int 
    {-# INLINE uniform #-}
    uniformR a b = pure (floor :: Double -> Word16) <*> uniformR (fromIntegral a) (fromIntegral b + 1)
    {-# INLINE uniformR #-}

instance Variate Word32 where
    uniform = pure fromIntegral <*> int 
    {-# INLINE uniform #-}
    uniformR a b = pure (floor :: Double -> Word32) <*> uniformR (fromIntegral a) (fromIntegral b + 1)
    {-# INLINE uniformR #-}

instance Variate Word64 where
#if (defined __GHCJS__ || WORD_SIZE_IN_BITS == 32)
    uniform = pure wordsTo64Bit <*> uniform <*> uniform
#else
    uniform = pure fromIntegral <*> int
#endif
    {-# INLINE uniform #-}
    uniformR a b = uniformRange (Proxy :: Proxy Word64) a b
    {-# INLINE uniformR #-}

instance Variate Word where
    uniform = pure fromIntegral <*> int
    {-# INLINE uniform #-}
    uniformR a b = uniformRange (Proxy :: Proxy Word64) a b
    {-# INLINE uniformR #-}

instance Variate Bool where
    uniform = pure wordToBool <*> uniform
    {-# INLINE uniform #-}
    uniformR a b
      | a == b = pure a
      | otherwise = uniform
    {-# INLINE uniformR #-}

instance Variate Float where
    uniform = pure wordToFloat <*> uniform
    {-# INLINE uniform #-}
    uniformR a b = pure (\w -> a + (b - a) * wordToFloat w) <*> uniform
    {-# INLINE uniformR #-}

instance Variate Double where
#if ( defined __GHCJS__ || WORD_SIZE_IN_BITS == 32 )
    uniformR a b = pure (\w1 w2 -> a + (b - a) * wordsToDouble w1 w2) <*> uniform <*> uniform
    {-# INLINE uniformR #-}
    uniform = pure wordsToDouble <*> uniform <*> uniform
    {-# INLINE uniform #-}
#else
    uniformR a b = pure (\w -> a + (b - a) * wordsToDouble (fromIntegral $ shiftR w 32) (fromIntegral w)) <*> (uniform :: Generator Word64)
    {-# INLINE uniformR #-}
    uniform = pure word64ToDouble <*> uniform
    {-# INLINE uniform #-}
#endif

{-# INLINE bool #-}
bool :: Generator Bool
bool = oneIn 2

{-# INLINE oneIn #-}
oneIn :: Int -> Generator Bool
oneIn n = pure (== 1) <*> uniformR 1 n

{-# INLINE sample #-}
sample :: Foldable f => f a -> Generator (Maybe a)
sample xs = 
    let !l = F.length xs
    in if l == 0 then pure Nothing else Generator (go l)
  where
    {-# INLINE go #-}
    go len seed = 
        let (!seed',!l) = generate (uniformR 0 len) seed
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
        let (!seed',!l) = generate (uniformR 0 len) seed
            x = V.unsafeIndex v l
        in (seed',Just x)

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
            let (seed',j) = generate (uniformR 0 (i + 1)) seed
            MV.unsafeSwap v i j
            go' seed' (i - 1)

{-# INLINE choose #-}
choose :: forall a. (Bounded a,Enum a) => Generator a
choose = pure toEnum <*> uniformR 0 (fromEnum (maxBound :: a))

{-# INLINE uniformRange #-}
uniformRange :: forall a b. 
                ( Integral a, Bounded a, Variate a
                , Integral b, Bounded b, Variate b
                ) => Proxy b -> a -> a -> Generator a
uniformRange _ x1 x2 = Generator go
  where
    {-# INLINE go #-}
    go seed
        | n == 0    = generate uniform seed -- Abuse overflow in unsigned types
        | otherwise = loop seed
        where
            (# i, j #) | x1 < x2   = (# x1, x2 #)
                       | otherwise = (# x2, x1 #)
            n = 1 + sub j i :: b
            buckets = maxBound `div` n
            maxN    = buckets * n
            {-# INLINE loop #-}
            loop seed = 
                let (!seed',!x) = generate uniform seed
                in if x < maxN 
                    then 
                        let !br = add i (fromIntegral (x `div` buckets))
                        in (seed',br)
                    else loop seed'

{-# INLINE independentSeed #-}
independentSeed :: Generator Seed
independentSeed = Generator go 
  where
    {-# INLINE go #-}
    go seed0 = 
        let !gen = uniformR 0 maxBound
            (!seed1,(!state,!b,!c)) = generate (pure (,,) <*> gen <*> gen <*> gen) seed0
            !incr = (b `xor` c) .|. 1
            !seed' = pcg_next (Seed state incr)
        in (seed',seed1)
