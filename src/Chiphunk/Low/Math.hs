-- | Description: Math helpers.
-- Module provides mathematics helper functions.
module Chiphunk.Low.Math where

import Data.VectorSpace

-- | Clamp @x@ to be between @a@ and @b@
fClamp
  :: Double -- ^ x
  -> Double -- ^ a
  -> Double -- ^ b
  -> Double
fClamp x a b
  | x < a     = a
  | x > b     = b
  | otherwise = x

-- | Linearly interpolate between @f1@ and @f2@
fLerp
  :: Double -- ^ f1
  -> Double -- ^ f2
  -> Double
  -> Double
fLerp = lerp

-- | Linearly interpolate from @x@ towards @y@ by no more than @d@.
fLerpConst
  :: Double -- ^ x
  -> Double -- ^ y
  -> Double -- ^ d
  -> Double
fLerpConst x y d = x + fClamp (y - x) (-d) d
