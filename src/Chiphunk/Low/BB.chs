module Chiphunk.Low.BB where

import Data.Fixed
import Foreign

import Chiphunk.Low.Math

{# import Chiphunk.Low.Types #}

#include <chipmunk/chipmunk.h>

bbNew :: Double -> Double -> Double -> Double -> BB
bbNew = BB

bbNewForExtents :: Vect -> Double -> Double -> BB
bbNewForExtents (Vect x y) hw hh = BB (x - hw) (y - hh) (x + hw) (y + hh)

bbNewForCircle :: Vect -> Double -> BB
bbNewForCircle v r = bbNewForExtents v r r

bbIntersects :: BB -> BB -> Bool
BB l1 b1 r1 t1 `bbIntersects` BB l2 b2 r2 t2 = r1 >= l1 && r2 >= l2 && t1 >= b1 && t2 >= b2

bbContains :: BB -> BB -> Bool
BB l1 b1 r1 t1 `bbContains` BB l2 b2 r2 t2 = l1 <= l2 && r1 >= r2 && t1 >= t2 && b1 <= b2

bbContainsVect :: BB -> Vect -> Bool
BB l b r t `bbContainsVect` Vect x y = l <= x && r >= x && b <= y && t >= y

bbMerge :: BB -> BB -> BB
BB l1 b1 r1 t1 `bbMerge` BB l2 b2 r2 t2 = BB (min l1 l2) (min b1 b2) (max r1 r2) (max t1 t2)

bbExpand :: BB -> Vect -> BB
BB l b r t `bbExpand` Vect x y = BB (min l x) (min b y) (max r x) (max t y)

bbCenter :: BB -> Vect
bbCenter (BB l b r t) = Vect ((l + r) / 2) ((b + t) / 2)

bbArea :: BB -> Double
bbArea (BB l b r t) = (r - l) * (t - b)

bbMergedArea :: BB -> BB -> Double
BB l1 b1 r1 t1 `bbMergedArea` BB l2 b2 r2 t2 = (max r1 r2 - min l1 l2) * (max t1 t2 - min b1 b2)

{# fun pure unsafe cpBBSegmentQuery as bbSegmentQuery {with* %`BB', with* %`Vect', with* %`Vect'} -> `Double' #}

{# fun pure unsafe cpBBIntersectsSegment as bbIntersectsSegment {with* %`BB', with* %`Vect', with* %`Vect'} -> `Bool' #}

bbClampVect :: BB -> Vect -> Vect
BB l b r t `bbClampVect` Vect x y = Vect (fClamp x l r) (fClamp y b t)

bbWrapVect :: BB -> Vect -> Vect
BB l b r t `bbWrapVect` Vect x y = Vect (l + ((x - l) `mod'` abs (r - l))) (b + ((y - b) `mod'` abs (t - b)))
