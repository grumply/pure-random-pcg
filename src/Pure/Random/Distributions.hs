{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module Pure.Random.Distributions where

import Pure.Random
import Pure.Random.PCG
import Pure.Random.PCG.Internal

import Control.Monad
import Data.Bits
import Data.Foldable
import Data.Traversable
import Data.Word

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

-- Distributions ported from Bryan O'Sullivan's mwc-random

{-# INLINE normal #-}
normal :: Double -> Double -> Generator Double
normal mean stdDev = pure (\x -> mean + stdDev * x) <*> standard

-- modified Doornik's ziggurat algorithm as implemented in mwc-random by Bryan O'Sullivan
{-# INLINE standard #-}
standard :: Generator Double
standard = loop
  where
    loop = do
      u  <- (subtract 1 . (*2)) `liftM` uniform
      ri <- uniform
      let i  = fromIntegral ((ri :: Word32) .&. 127)
          bi = U.unsafeIndex blocks i
          bj = U.unsafeIndex blocks (i+1)
      case () of
        _| abs u < U.unsafeIndex ratios i -> return $ u * bi
         | i == 0                         -> normalTail (u < 0)
         | otherwise                      -> do
             let x  = u * bi
                 xx = x * x
                 d  = exp (-0.5 * (bi * bi - xx))
                 e  = exp (-0.5 * (bj * bj - xx))
             c <- uniform
             if e + c * (d - e) < 1
               then return x
               else loop
    normalTail neg  = tailing
      where tailing  = do
              x <- ((/rNorm) . log) `liftM` uniform
              y <- log              `liftM` uniform
              if y * (-2) < x * x
                then tailing
                else return $! if neg then x - rNorm else rNorm - x

{-# INLINE exponential #-}
exponential :: Double -> Generator Double
exponential b = pure (\x -> - log x / b) <*> uniform

{-# INLINE truncatedExp #-}
truncatedExp :: Double -> (Double,Double) -> Generator Double
truncatedExp scale (a,b) = pure truncateExp <*> uniform
    where
        truncateExp p = a - log ( (1 - p) + p * exp (-scale * (b - a))) / scale

{-# INLINE gamma #-}
gamma :: Double -> Double -> Generator Double
gamma a b
  | a <= 0    = error "pure-random-pcg Pure.Random.Distributions.gamma: negative alpha parameter"
  | otherwise = mainloop
  where
    mainloop = do
      T x v <- innerloop
      u     <- uniform
      let cont =  u > 1 - 0.331 * sqr (sqr x)
              && log u > 0.5 * sqr x + a1 * (1 - v + log v)
      case () of
        _ | cont      -> mainloop
          | a >= 1    -> return $ a1 * v * b
          | otherwise -> do y <- uniform
                            return $ y ** (1 / a) * a1 * v * b
    innerloop = do
      x <- standard
      case 1 + a2*x of
        v | v <= 0    -> innerloop
          | otherwise -> return $ T x (v*v*v)

    a' = if a < 1 then a + 1 else a
    a1 = a' - 1/3
    a2 = 1 / sqrt(9 * a1)

{-# INLINE chiSquare #-}
chiSquare :: Int -> Generator Double
chiSquare n
  | n <= 0    = error "pure-random-pcg Pure.Random.Distributions.chiSquare: number of degrees of freedom must be positive"
  | otherwise = pure (\x -> 2 * x) <*> gamma (0.5 * fromIntegral n) 1 

{-# INLINE geometric0 #-}
geometric0 :: Double -> Generator Int
geometric0 p
  | p == 1          = pure 0
  | p >  0 && p < 1 = pure (\q -> floor $ log q / log (1 - p)) <*> uniform
  | otherwise       = error "pure-random-pcg Pure.Random.Distributions.geometric0: probability out of [0,1] range"

{-# INLINE geometric1 #-}
geometric1 :: Double -> Generator Int
geometric1 p = pure (+ 1) <*> geometric0 p

{-# INLINE beta #-}
beta :: Double -> Double -> Generator Double
beta a b = do
  x <- gamma a 1
  y <- gamma b 1
  return (x / (x+y))

{-# INLINE dirichlet #-}
dirichlet :: (Traversable t) => t Double -> Generator (t Double)
dirichlet t = do
  t' <- mapM (flip gamma 1) t
  let total = foldl' (+) 0 t'
  return $ fmap (/total) t'

{-# INLINE bernoulli #-}
bernoulli :: Double -> Generator Bool
bernoulli p = pure (< p) <*> uniform

{-# INLINE categorical #-}
categorical :: (G.Vector v Double) => v Double -> Generator Int
categorical v
    | G.null v = error "pure-random-pcg Pure.Random.Distributions.categorical: empty weights"
    | otherwise = do
        let cv  = G.scanl1' (+) v
        p <- (G.last cv *) `liftM` uniform
        return $ 
          case G.findIndex (>=p) cv of
            Just i  -> i
            Nothing -> error "pure-random-pcg Pure.Random.Distributions.categorical: bad weights"

{-# INLINE logCategorical #-}
logCategorical :: (G.Vector v Double) => v Double -> Generator Int
logCategorical v
  | G.null v  = error "pure-random-pcg Pure.Random.Distributions.logCategorical: empty weights"
  | otherwise = categorical (G.map (exp . subtract m) v)
  where
    m = G.maximum v

{-# INLINE sqr #-}
sqr :: Double -> Double
sqr x = x * x