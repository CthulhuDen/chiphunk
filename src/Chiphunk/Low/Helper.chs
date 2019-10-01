-- | Description: Helpers functions mostly for estimating certain measures.
-- Module provides helper function mostly for estimating certain measures.
module Chiphunk.Low.Helper
  ( momentForCircle
  , momentForSegment
  , momentForPoly
  , momentForBox
  , areaForCircle
  , areaForSegment
  , areaForPoly
  , centroidForPoly
  , convexHull
  , convexDecomposition
  ) where

import Data.VectorSpace
import Foreign
import System.IO.Unsafe

import Chiphunk.Low.Internal

{# import Chiphunk.Low.Types #}

#include <chipmunk/chipmunk.h>
#include <wrapper.h>

-- | Calculate the moment of inertia for a hollow circle, @r1@ and @r2@ are the inner and outer diameters
-- in no particular order. (A solid circle has an inner diameter of 0)
momentForCircle
  :: Double -- ^ Mass
  -> Double -- ^ r1
  -> Double -- ^ r2
  -> Vect   -- ^ Offset
  -> Double
momentForCircle m r1 r2 offs = m * (0.5 * (r1 * r1 + r2 * r2) + magnitudeSq offs)

-- | Calculate the moment of inertia for a line segment. The endpoints @a@ and @b@ are relative to the body.
momentForSegment
  :: Double -- ^ Mass
  -> Vect   -- ^ a
  -> Vect   -- ^ b
  -> Double -- ^ Thickness
  -> Double
momentForSegment m a b r = m * ((len * len + 4 * r * r) / 12 + magnitudeSq offs)
  where
    offs = lerp a b 0.5
    len  = magnitude (b ^-^ a) + 2 * r

-- | Calculate the moment of inertia for a solid polygon shape assuming its center of gravity is at its centroid.
-- The offset is added to each vertex.
{# fun pure unsafe cpMomentForPoly as momentForPoly
  { `Double'            -- ^ Mass
  , withList* `[Vect]'& -- ^ Vertexes
  , with* %`Vect'       -- ^ Offset
  , `Double'            -- ^ Thickness
  } -> `Double' #}

-- | Calculate the moment of inertia for a solid box centered on the body.
momentForBox
  :: Double -- ^ Mass
  -> Double -- ^ Width
  -> Double -- ^ Height
  -> Double
momentForBox m w h = m * (w * w + h * h) / 12

-- | Area of a hollow circle.
areaForCircle
  :: Double -- ^ r1
  -> Double -- ^ r2
  -> Double
areaForCircle r1 r2 = pi * abs (r1 * r1 - r2 * r2)

-- | Area of a beveled segment. (Will always be zero if radius is zero)
areaForSegment
  :: Vect   -- ^ One end
  -> Vect   -- ^ Other end
  -> Double -- ^ Thickness
  -> Double
areaForSegment v1 v2 r = magnitude (v1 ^-^ v2) * 2 * r + pi * r * r

-- | Signed area of a polygon shape. Returns a negative number for polygons with a clockwise winding.
{# fun pure unsafe cpAreaForPoly as areaForPoly
  { withList* `[Vect]'& -- ^ Vertexes
  , `Double'            -- ^ Thickness
  } -> `Double' #}

-- | Calculate the centroid for a polygon.
{# fun pure unsafe w_cpCentroidForPoly as centroidForPoly {withList* `[Vect]'&, alloca- `Vect' peek*} -> `()' #}

-- | Calculate the convex hull of a given set of points.
convexHull
  :: [Vect]        -- ^ Set of vertexes
  -> Double        -- ^ Allowed amount to shrink the hull when simplifying it. A tolerance of 0 creates an exact hull.
  -> ([Vect], Int) -- ^ Second element is index of first output vertex in input list.
convexHull vs tol = unsafePerformIO $
  withArray vs $ \pVs ->
  allocaArray (length vs) $ \pRes ->
  alloca $ \pFst -> do
    n <- {# call cpConvexHull as c_convexHull #} (fromIntegral $ length vs) pVs pRes pFst (realToFrac tol)
    (,) <$> peekArray (fromIntegral n) pRes <*> (fromIntegral <$> peek pFst)

convexDecomposition :: [Vect] -> Double -> [[Vect]]
convexDecomposition [] _ = []
convexDecomposition concavePolygon tol = unsafePerformIO $
  withPolylinePtr (Polyline counterClockwise) $ \lineP -> do
    setP <- {# call cpPolylineConvexDecomposition #} lineP (realToFrac tol)
    set <- peekPolylineSet setP
    {# call cpPolylineSetFree #} setP 1
    return $ map unPolyline $ unPolylineSet set
  where
    counterClockwise
      | areaForPoly concavePolygon 0 < 0 = reverse concavePolygon
      | otherwise                        = concavePolygon
