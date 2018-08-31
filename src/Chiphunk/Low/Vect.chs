-- | Description: 2D vector manipulations.
-- Module provides utilities to manipulate 2D vectors. Most of the code is re-implemented in Haskell (mirrorring C)
-- to avoid unnecessary foreign calls/marshalling, but only for simple operations.
--
-- Also note that 'Vect' has 'Eq', 'AdditiveGroup', 'VectorSpace', 'InnerSpace' and 'HasCross2' instances.
-- Large part of functions in this module just provides Chiphunk-compatible aliases for those instances' methods.
module Chiphunk.Low.Vect
  ( Vect (..)
  , cpv
  , vZero
  , vEql
  , vAdd
  , vSub
  , vNeg
  , vMult
  , vDot
  , vCross
  , vPerp
  , vRPerp
  , vProject
  , vRotate
  , vUnRotate
  , vLength
  , vLengthSq
  , vLerp
  , vLerpConst
  , vSLerp
  , vSLerpConst
  , vNormalize
  , vClamp
  , vDist
  , vDistSq
  , vNear
  , vForAngle
  , vToAngle
  ) where

import Data.Cross
import Data.VectorSpace
import Foreign

{# import Chiphunk.Low.Types #}

#include <wrapper.h>

-- | Convenience constructor for creating new cpVect structs.
-- Alias for 'Vect'
cpv :: Double -> Double -> Vect
cpv = Vect

-- | Constant for the zero vector.
--
-- Alias for 'zeroV'
vZero :: Vect
vZero = zeroV

-- | Check if two vectors are equal. (Be careful when comparing floating point numbers!)
--
-- Alias for '=='.
vEql :: Vect -> Vect -> Bool
vEql = (==)

-- | Add two vectors.
--
-- Alias for '^+^'.
vAdd :: Vect -> Vect -> Vect
vAdd = (^+^)

-- | Subtract two vectors.
--
-- Alias for '^-^'.
vSub :: Vect -> Vect -> Vect
vSub = (^-^)

-- | Negate a vector.
--
-- Alias for 'negateV'.
vNeg :: Vect -> Vect
vNeg = negateV

-- | Scalar multiplication.
--
-- Alias for '^*'.
vMult :: Vect -> Double -> Vect
vMult = (^*)

-- | Vector dot product.
--
-- Alias for '<.>'.
vDot :: Vect -> Vect -> Double
vDot = (<.>)

-- | 2D vector cross product analog. The cross product of 2D vectors results in a 3D vector with only a z component.
-- This function returns the value along the z-axis.
vCross :: Vect -> Vect -> Double
Vect x1 y1 `vCross` Vect x2 y2 = x1 * y2 - y1 * x2

-- | Returns a perpendicular vector. (90 degree rotation)
--
-- Alias for 'cross2'.
vPerp :: Vect -> Vect
vPerp = cross2

-- | Returns a perpendicular vector. (-90 degree rotation)
vRPerp :: Vect -> Vect
vRPerp v = negateV $ cross2 v

-- | Returns the vector projection of @v1@ onto @v2@.
--
-- Alias for 'project'.
vProject
  :: Vect -- ^ v1
  -> Vect -- ^ v2
  -> Vect
vProject = project

-- | Uses complex multiplication to rotate @v1@ by @v2@. Scaling will occur if @v1@ is not a unit vector.
vRotate
  :: Vect -- ^ v1
  -> Vect -- ^ v2
  -> Vect
Vect x1 y1 `vRotate` Vect x2 y2 = Vect (x1 * x2 - y1 * y2) (x1 * y2 + x2 * y1)

-- | Inverse of 'vRotate'.
vUnRotate :: Vect -> Vect -> Vect
Vect x1 y1 `vUnRotate` Vect x2 y2 = Vect (x1 * x2 + y1 * y2) (x2 * y1 - x1 * y2)

-- | Returns the length of v.
--
-- Alias for 'magnitude'.
vLength :: Vect -> Double
vLength = magnitude

-- | Returns the squared length of @v@. Faster than 'vLength' when you only need to compare lengths.
--
-- Alias for 'magnitudeSq'.
vLengthSq :: Vect -> Double
vLengthSq = magnitudeSq

-- | Linearly interpolate between @v1@ and @v2@.
--
-- Alias for 'lerp'.
vLerp
  :: Vect   -- ^ v1
  -> Vect   -- ^ v2
  -> Double
  -> Vect
vLerp = lerp

-- | Linearly interpolate between @v1@ towards @v2@ by distance @d@.
vLerpConst
  :: Vect   -- ^ v1
  -> Vect   -- ^ v2
  -> Double -- ^ d
  -> Vect
vLerpConst a b l = a ^+^ vClamp (b ^-^ a) l

-- | Spherical linearly interpolate between v1 and v2.
{# fun pure unsafe w_cpvslerp as vSLerp
  { with* %`Vect'        -- ^ v1
  , with* %`Vect'        -- ^ v2
  , `Double'
  , alloca- `Vect' peek*
  } -> `()' #}

-- | Spherical linearly interpolate between @v1@ towards @v2@ by no more than angle @a@ in radians.
{# fun pure unsafe w_cpvslerpconst as vSLerpConst
  { with* %`Vect'        -- ^ v1
  , with* %`Vect'        -- ^ v2
  , `Double'             -- ^ a
  , alloca- `Vect' peek*
  } -> `()' #}

-- | Returns a normalized copy of @v@. As a special case, it returns 'vZero' when called on 'vZero'.
--
-- Alias for 'normalized'.
vNormalize :: Vect -> Vect
vNormalize = normalized

-- | Clamp @v@ to length @len@.
vClamp
  :: Vect   -- ^ v
  -> Double -- ^ len
  -> Vect
vClamp v l
  | magnitudeSq v > l * l = l *^ normalized v
  | otherwise             = v

-- | Returns the distance between @v1@ and @v2@.
vDist
  :: Vect   -- ^ v1
  -> Vect   -- ^ v2
  -> Double
vDist v1 v2 = magnitude $ v1 ^-^ v2

-- | Returns the squared distance between @v1@ and @v2@. Faster than 'vDist' when you only need to compare distances.
vDistSq
  :: Vect   -- ^ v1
  -> Vect   -- ^ v2
  -> Double
vDistSq v1 v2 = magnitudeSq $ v1 ^-^ v2

-- | Returns true if the distance between @v1@ and @v2@ is less than @dist@.
vNear
  :: Vect   -- ^ v1
  -> Vect   -- ^ v2
  -> Double -- ^ dist
  -> Bool
vNear v1 v2 d = vDistSq v1 v2 < d * d

-- | Returns the unit length vector for the given angle (in radians).
vForAngle :: Double -> Vect
vForAngle alpha = Vect (cos alpha) (sin alpha)

-- | Returns the angular direction @v@ is pointing in (in radians).
vToAngle
  :: Vect   -- ^ v
  -> Double
vToAngle (Vect x y) = atan2 y x
