module Chiphunk.Low.Helper where

import Data.VectorSpace
import Foreign
import System.IO.Unsafe

import Chiphunk.Low.Internal

{# import Chiphunk.Low.Types #}

#include <chipmunk/chipmunk.h>
#include <wrapper.h>

momentForCircle :: Double -> Double -> Double -> Vect -> Double
momentForCircle m r1 r2 offs = m * (0.5 * (r1 * r1 + r2 * r2) + magnitudeSq offs)

momentForSegment :: Double -> Vect -> Vect -> Double -> Double
momentForSegment m a b r = m * ((len * len + 4 * r * r) / 12 + magnitudeSq offs)
  where
    offs = lerp a b 0.5
    len  = magnitude (b ^-^ a) + 2 * r

{# fun pure unsafe cpMomentForPoly as momentForPoly {`Double', withList* `[Vect]'&, with* %`Vect', `Double'} -> `Double' #}

momentForBox :: Double -> Double -> Double -> Double
momentForBox m w h = m * (w * w + h * h) / 12

areaForCircle :: Double -> Double -> Double
areaForCircle r1 r2 = pi * abs (r1 * r1 - r2 * r2)

areaForSegment :: Vect -> Vect -> Double -> Double
areaForSegment v1 v2 r = magnitude (v1 ^-^ v2) * 2 * r + pi * r * r

{# fun pure unsafe cpAreaForPoly as areaForPoly {withList* `[Vect]'&, `Double'} -> `Double' #}

{# fun pure unsafe w_cpCentroidForPoly as centroidForPoly {withList* `[Vect]'&, alloca- `Vect' peek*} -> `()' #}

convexHull :: [Vect] -> Double -> ([Vect], Int)
convexHull vs tol = unsafePerformIO $
  withArray vs $ \pVs ->
  allocaArray (length vs) $ \pRes ->
  alloca $ \pFst -> do
    n <- {# call cpConvexHull as c_convexHull #} (fromIntegral $ length vs) pVs pRes pFst (realToFrac tol)
    (,) <$> peekArray (fromIntegral n) pRes <*> (fromIntegral <$> peek pFst)
