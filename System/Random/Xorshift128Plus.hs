{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}

{-
Written in 2015 by Kanai Hiroki <kanai.hiroki12@gmail.com>
Originaly written by Sebastiano Vigna.

To the extent possible under law, the author has dedicated all copyright
and related and neighboring rights to this software to the public domain
worldwide. This software is distributed without any warranty.

See <http://creativecommons.org/publicdomain/zero/1.0/>.
-}

-- |
-- Module    : System.Random.Xorshift128Plus
-- License   : Public Commons
--
-- Maintainer  : kanai.hiroki12@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module is implementation of xorshift128+ random number generator.
-- Read <http://xorshift.di.unimi.it/xorshift128plus.c original implementation and description>
-- is strictly recommended.
--
-- The generator state is stored in the 'Gen' data type and
-- it is created by 'initialize' function with seed value or
-- calling 'Gen' data constructor with generator state.
--
-- To generate random values, first 'initialize' a random number
-- generator by seed value.
--
-- @
--   let gen = 'initialize' 3748374974327
-- @
--
-- Then generate a random number by 'next' or 'next01'.
--
-- @
--   let (v, gen') = 'next' gen
-- @
--
-- @
--   let (v, gen') = 'next01' gen
-- @
module System.Random.Xorshift128Plus(
  Gen(..),
  initialize,
  next,
  next01,
  next#
  ) where

import Data.Word(Word64(..))
import Data.Bits

-- | Random number generator.
data Gen = Gen {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
         deriving (Eq,Show)

-- | Create a generator by seed value. Please do not supply 0.
initialize :: Word64 -> Gen
initialize s = let s0 = 1812433253 * (s `xor` (s `shiftR` 30)) + 1
                   s1 = 1812433253 * (s0 `xor` (s0 `shiftR` 30)) + 2
               in  Gen s0 s1
{-# INLINE initialize #-}

-- | Generate a 64bit random value and next generator state.
next :: Gen -> (Word64, Gen)
next g = let (# n, g' #) = next# g
         in  (n, g')
{-# INLINE next #-}

-- | Generate a random value between 0 and 1. And next generator state.
next01 :: Gen -> (Double, Gen)
next01 g = let (# n, g' #) = next# g
           in (fromIntegral n / word64max, g')
{-# INLINE next01 #-}

-- | Same as 'next', but return values with unboxed tuple.
next# :: Gen -> (# Word64, Gen #)
next# g = let g'@(Gen s0 s1) = step g
          in  (# (s0 + s1), g' #)
{-# INLINE next# #-}

step :: Gen -> Gen
step (Gen s0 s1) =
  let s1' = s0 `xor` (s0 `shiftL` 23)
  in  Gen s1 (s1' `xor` s1 `xor` (s1' `shiftR` 17) `xor` (s1 `shiftR` 26))
{-# INLINE step #-}

word64max :: Double
word64max = fromIntegral (maxBound :: Word64)
{-# INLINE word64max #-}

-- TODO: unit test
-- test
-- main :: IO ()
-- main = do
--   let g = initialize 3748374974327
--       (# d1, g1 #) = next01# g
--       (# d2, g2 #) = next01# g1
--       (# d3, g3 #) = next01# g2
--   print (d1, d2, d3) -- => (0.4837001127332096,8.873189590555798e-2,0.7393015945448153)
