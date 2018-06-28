{-# LANGUAGE MagicHash, BangPatterns, CPP #-}
module Pure.Random.PCG.Internal where

import Data.Bits ((.&.),xor)
import GHC.Base (Int(..),uncheckedIShiftRL#)

import qualified System.Random

#ifndef __GHCJS__
{-# INLINE newSeed #-}
newSeed :: IO Seed
newSeed = do
   gen <- System.Random.newStdGen
   let (i,_) = System.Random.random gen
   return (initialSeed i)
#else
foreign import javascript unsafe 
  "Math.floor(Math.random()*0xFFFFFFFF)" random_seed_js :: IO Int
newSeed :: IO Seed
newSeed = initialSeed <$> random_seed_js
#endif

data Seed = Seed {-# UNPACK #-}!Int {-# UNPACK #-}!Int 
  deriving (Eq,Show)

{-# INLINE uncheckedIShiftRL #-}
uncheckedIShiftRL :: Int -> Int -> Int 
uncheckedIShiftRL (I# n) (I# i) = I# (uncheckedIShiftRL# n i)

{-# INLINE pcg_step #-}
pcg_step :: Seed -> Seed
pcg_step (Seed state incr) = Seed (state * 2891336453 + incr) incr

{-# INLINE pcg_advance #-}
pcg_advance :: Seed -> Int -> Seed
pcg_advance (Seed state incr) delta = Seed (pcg_advance_lcg state delta 747796405 incr) incr

{-# INLINE pcg_advance_lcg #-}
pcg_advance_lcg :: Int -> Int -> Int -> Int -> Int
pcg_advance_lcg state delta cur_mult cur_plus = go cur_mult cur_plus 1 0 delta
  where
    go !cur_mult !cur_plus !acc_mult !acc_plus !delta
      | delta > 0 =
        if (delta .&. 1 /= 0) 
            then go (cur_mult * cur_mult) ((cur_mult + 1) * cur_plus) (acc_mult * cur_mult) (acc_plus * cur_mult + cur_plus) (delta `div` 2)
            else go (cur_mult * cur_mult) ((cur_mult + 1) * cur_plus) acc_mult acc_plus (delta `div` 2)
      | otherwise = acc_mult * state + acc_plus

{-# INLINE pcg_next #-}
pcg_next :: Seed -> Seed
pcg_next (Seed state0 incr) = Seed (state0 * 1664525 + incr) incr

{-# INLINE pcg_peel #-}
pcg_peel :: Seed -> Int
pcg_peel (Seed state _) =
    let
        !word = 
            xor state (uncheckedIShiftRL state (uncheckedIShiftRL state 28 + 4)) * 277803737

        !result = xor (uncheckedIShiftRL word 22) word

    in
        result

{-# INLINE initialSeed #-}
initialSeed :: Int -> Seed
initialSeed x =
    let 
        (Seed state0 incr) = pcg_next (Seed 0 1013904223)

        !state1 = state0 + x

        !result = pcg_next (Seed state1 incr)

    in 
        result

