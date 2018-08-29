{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}

module Chiphunk
  ( CPVect (..)
  , cpv
  ) where

import Foreign
import Foreign.C.Types
import System.IO.Unsafe

#include <chipmunk/chipmunk.h>
#include <wrapper.h>

type CPFloat = {# type cpFloat #}

data CPVect = CPVect
  { cpvX :: Double, cpvY :: Double }
  deriving Show

{# pointer *cpVect foreign -> CPVect nocode #}

instance Storable CPVect where
  sizeOf _    = {# sizeof cpVect #}
  alignment _ = {# alignof cpVect #}
  poke p (CPVect x y) = do
    {# set cpVect->x #} p $ realToFrac @_ @CPFloat x
    {# set cpVect->y #} p $ realToFrac @_ @CPFloat y
  peek p = CPVect <$> (realToFrac @CPFloat <$> {# get cpVect->x #} p)
                  <*> (realToFrac @CPFloat <$> {# get cpVect->y #} p)

{# fun pure unsafe w_cpv as cpv {`Double' , `Double', alloca- `CPVect' peek*} -> `()' #}
