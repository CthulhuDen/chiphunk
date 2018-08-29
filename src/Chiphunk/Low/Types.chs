{-# LANGUAGE TypeFamilies #-}

module Chiphunk.Low.Types
  ( Vect (..)
  , VectPtr
  , SpacePtr
  , BodyPtr
  , ShapePtr
  ) where

import Data.Cross
import Data.VectorSpace
import Foreign

#include <chipmunk/chipmunk.h>

data Vect = Vect
  { cpvX :: !Double, cpvY :: !Double }
  deriving (Eq, Show)

instance AdditiveGroup Vect where
  zeroV = Vect 0 0
  negateV (Vect x y) = Vect (-x) (-y)
  Vect x1 y1 ^+^ Vect x2 y2 = Vect (x1 + x2) (y1 + y2)
  Vect x1 y1 ^-^ Vect x2 y2 = Vect (x1 - x2) (y1 - y2)

instance VectorSpace Vect where
  type Scalar Vect = Double
  f *^ Vect x y = Vect (f * x) (f * y)

instance InnerSpace Vect where
  Vect x1 y1 <.> Vect x2 y2 = x1 * x2 + y1 * y2

instance HasCross2 Vect where
  cross2 (Vect x y) = Vect (-y) x

{# pointer *cpVect as VectPtr -> Vect #}

instance Storable Vect where
  sizeOf _    = {# sizeof cpVect #}
  alignment _ = {# alignof cpVect #}
  poke p (Vect x y) = do
    {# set cpVect->x #} p $ realToFrac x
    {# set cpVect->y #} p $ realToFrac y
  peek p = Vect <$> (realToFrac <$> {# get cpVect->x #} p)
                <*> (realToFrac <$> {# get cpVect->y #} p)

data Space

{# pointer *cpSpace as SpacePtr -> Space #}

data Body

{# pointer *cpBody as BodyPtr -> Body #}

data Shape

{# pointer *cpShape as ShapePtr -> Shape #}
