-- | Description: Math helpers.
-- Module provides mathematics helper functions.
module Chiphunk.Low.Math where

import Data.VectorSpace

-- | Clamp @f@ to be between @min@ and @max@
fClamp
  :: Double -- ^ f
  -> Double -- ^ min
  -> Double -- ^ max
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

-- | Linearly interpolate from @f1@ towards @f2@ by no more than @d@.
fLerpConst
  :: Double -- ^ f1
  -> Double -- ^ f2
  -> Double -- ^ d
  -> Double
fLerpConst x y d = x + fClamp (y - x) (-d) d
