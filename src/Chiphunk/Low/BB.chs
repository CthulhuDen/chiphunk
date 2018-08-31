-- | Description: Utilities for working with bounding box.
-- Module provides utilities for working with bounding box.
module Chiphunk.Low.BB
  ( BB (..)
  , bbNew
  , bbNewForExtents
  , bbNewForCircle
  , bbIntersects
  , bbContains
  , bbContainsVect
  , bbMerge
  , bbExpand
  , bbCenter
  , bbArea
  , bbMergedArea
  , bbSegmentQuery
  , bbIntersectsSegment
  , bbClampVect
  , bbWrapVect
  ) where

import Data.Fixed
import Foreign

import Chiphunk.Low.Math

{# import Chiphunk.Low.Types #}

#include <chipmunk/chipmunk.h>

-- | Convenience constructor for 'BB' structs. Like 'cpv' this function returns a copy and not a malloced pointer.
bbNew :: Double -> Double -> Double -> Double -> BB
bbNew = BB

-- | Convenience constructor for making a 'BB' fitting with a center point and half width and height.
bbNewForExtents
  :: Vect   -- ^ Center point
  -> Double -- ^ Half width
  -> Double -- ^ Half height
  -> BB
bbNewForExtents (Vect x y) hw hh = BB (x - hw) (y - hh) (x + hw) (y + hh)

-- | Convenience constructor for making a 'BB' fitting a circle at position @p@ with radius @r@.
bbNewForCircle
  :: Vect   -- ^ p
  -> Double -- ^ r
  -> BB
bbNewForCircle v r = bbNewForExtents v r r

-- | Returns true if the bounding boxes intersect.
bbIntersects :: BB -> BB -> Bool
BB l1 b1 r1 t1 `bbIntersects` BB l2 b2 r2 t2 = r1 >= l1 && r2 >= l2 && t1 >= b1 && t2 >= b2

-- | Returns true if @bb@ completely contains @other@.
bbContains
  :: BB   -- ^ bb
  -> BB   -- ^ other
  -> Bool
BB l1 b1 r1 t1 `bbContains` BB l2 b2 r2 t2 = l1 <= l2 && r1 >= r2 && t1 >= t2 && b1 <= b2

-- | Returns true if @bb@ contains @v@.
bbContainsVect
  :: BB   -- ^ bb
  -> Vect -- ^ v
  -> Bool
BB l b r t `bbContainsVect` Vect x y = l <= x && r >= x && b <= y && t >= y

-- | Return the minimal bounding box that contains both @a@ and @b@.
bbMerge
  :: BB -- ^ a
  -> BB -- ^ b
  -> BB
BB l1 b1 r1 t1 `bbMerge` BB l2 b2 r2 t2 = BB (min l1 l2) (min b1 b2) (max r1 r2) (max t1 t2)

-- | Return the minimal bounding box that contains both @bb@ and @v@.
bbExpand
  :: BB   -- ^ bb
  -> Vect -- ^ v
  -> BB
BB l b r t `bbExpand` Vect x y = BB (min l x) (min b y) (max r x) (max t y)

-- | Return the center of @bb@.
bbCenter
  :: BB   -- ^ bb
  -> Vect
bbCenter (BB l b r t) = Vect ((l + r) / 2) ((b + t) / 2)

-- | Return the area of @bb@.
bbArea
  :: BB     -- ^ bb
  -> Double
bbArea (BB l b r t) = (r - l) * (t - b)

-- | Merges @a@ and @b@ then returns the area of the merged bounding box.
bbMergedArea
  :: BB     -- ^ a
  -> BB     -- ^ b
  -> Double
BB l1 b1 r1 t1 `bbMergedArea` BB l2 b2 r2 t2 = (max r1 r2 - min l1 l2) * (max t1 t2 - min b1 b2)

-- | Returns the fraction along the segment query the 'BB' is hit. Returns INFINITY if it doesn’t hit.
{# fun pure unsafe cpBBSegmentQuery as bbSegmentQuery
  { with* %`BB'   -- ^ Box
  , with* %`Vect' -- ^ One segment end
  , with* %`Vect' -- ^ Other segment end
  } -> `Double' #}

-- | Returns true if the segment defined by endpoints @a@ and @b@ intersect @bb@.
{# fun pure unsafe cpBBIntersectsSegment as bbIntersectsSegment
  { with* %`BB'   -- ^ bb
  , with* %`Vect' -- ^ a
  , with* %`Vect' -- ^ b
  } -> `Bool' #}

-- | Returns a copy of @v@ clamped to the bounding box @bb@.
bbClampVect
  :: BB   -- ^ bb
  -> Vect -- ^ v
  -> Vect
BB l b r t `bbClampVect` Vect x y = Vect (fClamp x l r) (fClamp y b t)

-- | Returns a copy of @v@ wrapped to the bounding box @bb@.
bbWrapVect
  :: BB   -- ^ bb
  -> Vect -- ^ v
  -> Vect
BB l b r t `bbWrapVect` Vect x y = Vect (l + ((x - l) `mod'` abs (r - l))) (b + ((y - b) `mod'` abs (t - b)))
