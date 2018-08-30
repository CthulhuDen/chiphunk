module Chiphunk.Low.Vect where

import Data.Cross
import Data.VectorSpace
import Foreign

{# import Chiphunk.Low.Types #}

#include <wrapper.h>

cpv :: Double -> Double -> Vect
cpv = Vect

vEql :: Vect -> Vect -> Bool
vEql = (==)

vAdd :: Vect -> Vect -> Vect
vAdd = (^+^)

vSub :: Vect -> Vect -> Vect
vSub = (^-^)

vNeg :: Vect -> Vect
vNeg = negateV

vMult :: Vect -> Double -> Vect
vMult = (^*)

vDot :: Vect -> Vect -> Double
vDot = (<.>)

vCross :: Vect -> Vect -> Double
Vect x1 y1 `vCross` Vect x2 y2 = x1 * y2 - y1 * x2

vPerp :: Vect -> Vect
vPerp v = cross2 v

vRPerp :: Vect -> Vect
vRPerp v = negateV $ cross2 v

vProject :: Vect -> Vect -> Vect
vProject = project

vRotate :: Vect -> Vect -> Vect
Vect x1 y1 `vRotate` Vect x2 y2 = Vect (x1 * x2 - y1 * y2) (x1 * y2 + x2 * y1)

vUnRotate :: Vect -> Vect -> Vect
Vect x1 y1 `vUnRotate` Vect x2 y2 = Vect (x1 * x2 + y1 * y2) (x2 * y1 - x1 * y2)

vLength :: Vect -> Double
vLength = magnitude

vLengthSq :: Vect -> Double
vLengthSq = magnitudeSq

vLerpConst :: Vect -> Vect -> Double -> Vect
vLerpConst a b l = a ^+^ vClamp (b ^-^ a) l

{# fun pure unsafe w_cpvslerp as vSLerp {with* %`Vect', with* %`Vect', `Double', alloca- `Vect' peek*} -> `()' #}

{# fun pure unsafe w_cpvslerpconst as vSLerpConst {with* %`Vect', with* %`Vect', `Double', alloca- `Vect' peek*} -> `()' #}

vNormalize :: Vect -> Vect
vNormalize = normalized

vClamp :: Vect -> Double -> Vect
vClamp v l
  | magnitudeSq v > l * l = l *^ normalized v
  | otherwise             = v

vDist :: Vect -> Vect -> Double
vDist v1 v2 = magnitude $ v1 ^-^ v2

vDistSq :: Vect -> Vect -> Double
vDistSq v1 v2 = magnitudeSq $ v1 ^-^ v2

vNear :: Vect -> Vect -> Double -> Bool
vNear v1 v2 d = vDistSq v1 v2 < d * d

vForAngle :: Double -> Vect
vForAngle alpha = Vect (cos alpha) (sin alpha)

vToAngle :: Vect -> Double
vToAngle (Vect x y) = atan2 y x
