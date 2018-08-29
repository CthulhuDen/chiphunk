{-# LANGUAGE ForeignFunctionInterface #-}

module Chiphunk
  ( CPVect (..)
  , cpv
  , cpSpaceNew
  , cpSpaceSetGravity
  , cpSpaceGetStaticBody
  , cpBodyNew
  , cpSegmentShapeNew
  , cpCircleShapeNew
  , cpMomentForCircle
  , cpShapeSetFriction
  , cpSpaceAddShape
  , cpSpaceAddBody
  , cpBodySetPosition
  , cpBodyGetPosition
  , cpBodyGetVelocity
  , cpSpaceStep

  , cpBodyFree
  , cpShapeFree
  , cpSpaceFree
  ) where

import Foreign
import Foreign.C.Types
import System.IO.Unsafe

#include <chipmunk/chipmunk.h>
#include <wrapper.h>

data CPVect = CPVect
  { cpvX :: Double, cpvY :: Double }
  deriving Show

{# pointer *cpVect as CPVectPtr -> CPVect #}

instance Storable CPVect where
  sizeOf _    = {# sizeof cpVect #}
  alignment _ = {# alignof cpVect #}
  poke p (CPVect x y) = do
    {# set cpVect->x #} p $ realToFrac x
    {# set cpVect->y #} p $ realToFrac y
  peek p = CPVect <$> (realToFrac <$> {# get cpVect->x #} p)
                  <*> (realToFrac <$> {# get cpVect->y #} p)

{# fun pure unsafe w_cpv as cpv {`Double' , `Double', alloca- `CPVect' peek*} -> `()' #}

data CPSpace

{# pointer *cpSpace as CPSpacePtr -> CPSpace #}

data CPBody

{# pointer *cpBody as CPBodyPtr -> CPBody #}

data CPShape

{# pointer *cpShape as CPShapePtr -> CPShape #}

{# fun unsafe cpSpaceNew {} -> `CPSpacePtr' #}

{# fun unsafe cpSpaceSetGravity {`CPSpacePtr', with* %`CPVect'} -> `()' #}

{# fun unsafe w_cpSpaceGetGravity as cpSpaceGetGravity {`CPSpacePtr', alloca- `CPVect' peek*} -> `()' #}

{# fun unsafe cpSpaceGetStaticBody {`CPSpacePtr'} -> `CPBodyPtr' #}

{# fun unsafe cpBodyNew {`Double', `Double'} -> `CPBodyPtr' #}

{# fun unsafe cpSegmentShapeNew {`CPBodyPtr', with* %`CPVect', with* %`CPVect', `Double'} -> `CPShapePtr' #}

{# fun unsafe cpCircleShapeNew {`CPBodyPtr', `Double', with* %`CPVect'} -> `CPShapePtr' #}

{# fun pure unsafe cpMomentForCircle {`Double', `Double', `Double', with* %`CPVect'} -> `Double' #}

{# fun unsafe cpShapeSetFriction {`CPShapePtr', `Double'} -> `()' #}

{# fun unsafe cpSpaceAddShape {`CPSpacePtr', `CPShapePtr'} -> `()' #}

{# fun unsafe cpSpaceAddBody {`CPSpacePtr', `CPBodyPtr'} -> `()' #}

{# fun unsafe cpBodySetPosition {`CPBodyPtr', with* %`CPVect'} -> `()' #}

{# fun unsafe w_cpBodyGetPosition as cpBodyGetPosition {`CPBodyPtr', alloca- `CPVect' peek*} -> `()' #}

{# fun unsafe w_cpBodyGetVelocity as cpBodyGetVelocity {`CPBodyPtr', alloca- `CPVect' peek*} -> `()' #}

{# fun unsafe cpSpaceStep {`CPSpacePtr', `Double'} -> `()' #}

{# fun unsafe cpBodyFree {`CPBodyPtr'} -> `()' #}

{# fun unsafe cpShapeFree {`CPShapePtr'} -> `()' #}

{# fun unsafe cpSpaceFree {`CPSpacePtr'} -> `()' #}
