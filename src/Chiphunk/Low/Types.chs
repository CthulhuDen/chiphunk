{-# LANGUAGE TypeFamilies #-}

module Chiphunk.Low.Types
  ( Vect (..)
  , VectPtr
  , BB (..)
  , BBPtr
  , SpacePtr
  , BodyPtr
  , ShapePtr
  ) where

import Data.Cross
import Data.VectorSpace
import Foreign

#include <chipmunk/chipmunk.h>

data Vect = Vect
  { vX :: !Double, vY :: !Double
  } deriving (Eq, Show)

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

instance Storable Vect where
  sizeOf _    = {# sizeof cpVect #}
  alignment _ = {# alignof cpVect #}
  poke p (Vect x y) = do
    {# set cpVect->x #} p $ realToFrac x
    {# set cpVect->y #} p $ realToFrac y
  peek p = Vect <$> (realToFrac <$> {# get cpVect->x #} p)
                <*> (realToFrac <$> {# get cpVect->y #} p)

{# pointer *cpVect as VectPtr -> Vect #}

data BB = BB
  { bbL :: !Double, bbB :: !Double, bbR :: !Double, bbT :: !Double
  } deriving (Show)

instance Storable BB where
  sizeOf _    = {# sizeof cpBB #}
  alignment _ = {# alignof cpBB #}
  poke p (BB l b r t) = do
    {# set cpBB->l #} p $ realToFrac l
    {# set cpBB->b #} p $ realToFrac b
    {# set cpBB->r #} p $ realToFrac r
    {# set cpBB->t #} p $ realToFrac t
  peek p = BB <$> (realToFrac <$> {# get cpBB->l #} p)
              <*> (realToFrac <$> {# get cpBB->b #} p)
              <*> (realToFrac <$> {# get cpBB->r #} p)
              <*> (realToFrac <$> {# get cpBB->t #} p)

{# pointer *cpBB as BBPtr -> BB #}

data Space

{# pointer *cpSpace as SpacePtr -> Space #}

data Body

{# pointer *cpBody as BodyPtr -> Body #}

data Shape

{# pointer *cpShape as ShapePtr -> Shape #}
