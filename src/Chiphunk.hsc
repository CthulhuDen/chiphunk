{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}

module Chiphunk where

import Foreign
import Foreign.C.Types
import System.IO.Unsafe

#include "chipmunk/chipmunk.h"
#include "wrapper.h"

type CPFloat = CDouble

data CPVect = CPVect
  { cpvX :: Double, cpvY :: Double }
  deriving Show

instance Storable CPVect where
  sizeOf _    = #{size cpVect}
  alignment _ = #{alignment cpVect}
  poke p (CPVect x y) = do
    #{poke cpVect, x} p $ realToFrac @_ @CPFloat x
    #{poke cpVect, y} p $ realToFrac @_ @CPFloat y
  peek p = CPVect <$> (realToFrac @CPFloat <$> #{peek cpVect, x} p)
                  <*> (realToFrac @CPFloat <$> #{peek cpVect, y} p)


foreign import ccall unsafe "wrapper.h w_cpv"
  c_cpv :: CPFloat -> CPFloat -> Ptr CPVect -> IO CInt

cpv :: Double -> Double -> CPVect
cpv x y = unsafePerformIO $
  alloca $ \t -> c_cpv (realToFrac x) (realToFrac y) t *> peek t
