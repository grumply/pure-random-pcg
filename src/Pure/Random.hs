{-# LANGUAGE CPP, FlexibleContexts, TypeFamilies, ScopedTypeVariables, BangPatterns, MagicHash, UnboxedTuples #-}
module Pure.Random where

import Data.Bits
import Data.Foldable as F
import Data.Int
import Data.Word

import Pure.Random.PCG
import Pure.Random.PCG.Internal

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

#ifndef __GHCJS__
#import "MachDeps.h"
#endif

-- This module is a port of Bryan O'Sullivan's mwc-random

word32 :: Generator Word32
word32 = pure fromIntegral <*> int

wordToFloat :: Word32 -> Float
wordToFloat x      = (fromIntegral i * m_inv_32) + 0.5 + m_inv_33
    where m_inv_33 = 1.16415321826934814453125e-10
          m_inv_32 = 2.3283064365386962890625e-10
          i        = fromIntegral x :: Int32
{-# INLINE wordToFloat #-}

wordsToDouble :: Word32 -> Word32 -> Double
wordsToDouble x y  = (fromIntegral u * m_inv_32 + (0.5 + m_inv_53) +
                     fromIntegral (v .&. 0xFFFFF) * m_inv_52)
    where m_inv_52 = 2.220446049250313080847263336181640625e-16
          m_inv_53 = 1.1102230246251565404236316680908203125e-16
          m_inv_32 = 2.3283064365386962890625e-10
          u        = fromIntegral x :: Int32
          v        = fromIntegral y :: Int32
{-# INLINE wordsToDouble #-}

word64ToDouble :: Word64 -> Double
word64ToDouble x = (fromIntegral u * m_inv_32 + (0.5 + m_inv_53) +
                   fromIntegral (v .&. 0xFFFFF) * m_inv_52)
    where m_inv_52 = 2.220446049250313080847263336181640625e-16
          m_inv_53 = 1.1102230246251565404236316680908203125e-16
          m_inv_32 = 2.3283064365386962890625e-10
          u        = fromIntegral (shiftR x 32) :: Int32
          v        = fromIntegral x :: Int32
{-# INLINE word64ToDouble #-}

type family Unsigned a :: *
type instance Unsigned Int8   = Word8
type instance Unsigned Int16  = Word16
type instance Unsigned Int32  = Word32
type instance Unsigned Int64  = Word64
type instance Unsigned Int    = Word
type instance Unsigned Word8  = Word8
type instance Unsigned Word16 = Word16
type instance Unsigned Word32 = Word32
type instance Unsigned Word64 = Word64
type instance Unsigned Word   = Word

sub :: (Integral a, Integral (Unsigned a)) => a -> a -> Unsigned a
sub x y = fromIntegral x - fromIntegral y
{-# INLINE sub #-}

add :: (Integral a, Integral (Unsigned a)) => a -> Unsigned a -> a
add m x = m + fromIntegral x
{-# INLINE add #-}

uniformRange :: ( Integral a, Bounded a, Variate a
                , Integral (Unsigned a), Bounded (Unsigned a), Variate (Unsigned a))
             => a -> a -> Generator a
uniformRange x1 x2
  | n == 0    = uniform -- Abuse overflow in unsigned types
  | otherwise = loop
  where
    -- Allow ranges where x2<x1
    (# i, j #) | x1 < x2   = (# x1, x2 #)
               | otherwise = (# x2, x1 #)
    n       = 1 + sub j i
    buckets = maxBound `div` n
    maxN    = buckets * n
    loop    = do x <- uniform
                 if x < maxN then return $! add i (x `div` buckets)
                             else loop
{-# INLINE uniformRange #-}

wordsTo64Bit :: (Integral a) => Word32 -> Word32 -> a
wordsTo64Bit x y =
    fromIntegral ((fromIntegral x `shiftL` 32) .|. fromIntegral y :: Word64)
{-# INLINE wordsTo64Bit #-}

wordToBool :: Word32 -> Bool
wordToBool i = (i .&. 1) /= 0

class Variate a where
    uniform :: Generator a
    uniformR :: a -> a -> Generator a

instance Variate Int8 where
    uniform = pure fromIntegral <*> int
    uniformR a b = uniformRange a b

instance Variate Int16 where
    uniform = pure fromIntegral <*> int
    uniformR a b = uniformRange a b

instance Variate Int32 where
    uniform = pure fromIntegral <*> int
    uniformR a b = uniformRange a b

instance Variate Int64 where
#if (defined __GHCJS__ || WORD_SIZE_IN_BITS == 32)
    uniform = pure wordsTo64Bit <*> uniform <*> uniform
#else
    uniform = pure fromIntegral <*> int
#endif
    uniformR a b = uniformRange a b

instance Variate Int where
    uniform = int
    uniformR a b = uniformRange a b

instance Variate Word8 where
    uniform = pure fromIntegral <*> int 
    uniformR a b = uniformRange a b

instance Variate Word16 where
    uniform = pure fromIntegral <*> int 
    uniformR a b = uniformRange a b

instance Variate Word32 where
    uniform = pure fromIntegral <*> int 
    uniformR a b = uniformRange a b

instance Variate Word64 where
#if (defined __GHCJS__ || WORD_SIZE_IN_BITS == 32)
    uniform = pure wordsTo64Bit <*> uniform <*> uniform
#else
    uniform = pure fromIntegral <*> int
#endif
    uniformR a b = uniformRange a b

instance Variate Word where
    uniform = pure fromIntegral <*> int
    uniformR a b = uniformRange a b

instance Variate Bool where
    uniform = pure wordToBool <*> uniform
    uniformR a b
      | a == b = pure a
      | otherwise = uniform

instance Variate Float where
    uniform = pure wordToFloat <*> uniform
    uniformR a b = pure (\w -> a + (b - a) * wordToFloat w) <*> uniform

instance Variate Double where
#if ( defined __GHCJS__ || WORD_SIZE_IN_BITS == 32 )
    uniformR a b = pure (\w1 w2 -> a + (b - a) * wordsToDouble w1 w2) <*> uniform <*> uniform
    uniform = pure wordsToDouble <*> uniform <*> uniform
#else
    uniformR a b = pure (\w -> a + (b - a) * wordsToDouble (fromIntegral $ shiftR w 32) (fromIntegral w)) <*> (uniform :: Generator Word64)
    uniform = pure word64ToDouble <*> uniform
#endif

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
        let (!seed',!l) = generate (boundedRand len) seed
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
        let (!seed',!l) = generate (boundedRand len) seed
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
            let (seed',j) = generate (intR 0 (i + 1)) seed
            MV.unsafeSwap v i j
            go' seed' (i - 1)

{-# INLINE choose #-}
choose :: forall a. (Bounded a,Enum a) => Generator a
choose = pure toEnum <*> intR 0 (fromEnum (maxBound :: a))

