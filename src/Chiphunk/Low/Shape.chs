module Chiphunk.Low.Shape where

import Foreign

import Chiphunk.Low.Internal

{# import Chiphunk.Low.Types #}

#include <chipmunk/chipmunk.h>
#include <wrapper.h>

{# fun unsafe cpShapeGetBody as shapeGetBody {`ShapePtr'} -> `BodyPtr' #}

{# fun unsafe cpShapeSetBody as shapeSetBody {`ShapePtr', `BodyPtr'} -> `()' #}

{# fun unsafe w_cpShapeGetBB as shapeGetBB {`ShapePtr', alloca- `BB' peek*} -> `()' #}

{# fun unsafe cpShapeGetSensor as shapeGetSensor {`ShapePtr'} -> `Bool' #}

{# fun unsafe cpShapeSetSensor as shapeSetSensor {`ShapePtr', `Bool'} -> `()' #}

{# fun unsafe cpShapeGetElasticity as shapeGetElasticity {`ShapePtr'} -> `Double' #}

{# fun unsafe cpShapeSetElasticity as shapeSetElasticity {`ShapePtr', `Double'} -> `()' #}

{# fun unsafe cpShapeGetFriction as shapeGetFriction {`ShapePtr'} -> `Double' #}

{# fun unsafe cpShapeSetFriction as shapeSetFriction {`ShapePtr', `Double'} -> `()' #}

{# fun unsafe w_cpShapeGetSurfaceVelocity as shapeGetSurfaceVelocity {`ShapePtr', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpShapeSetSurfaceVelocity as shapeSetSurfaceVelocity {`ShapePtr', with* %`Vect'} -> `()' #}

{# fun unsafe cpShapeGetCollisionType as shapeGetCollisionType {`ShapePtr'} -> `WordPtr' fromIntegral #}

{# fun unsafe cpShapeSetCollisionType as shapeSetCollisionType {`ShapePtr', fromIntegral `WordPtr'} -> `()' #}

data ShapeFilter = ShapeFilter
  { sfGroup :: !WordPtr
  , sfCategories :: !Word32
  , sfMask :: !Word32
  } deriving Show

instance Storable ShapeFilter where
  sizeOf _    = {# sizeof cpShapeFilter #}
  alignment _ = {# alignof cpShapeFilter #}
  poke p (ShapeFilter g c m) = do
    {# set cpShapeFilter->group #} p      $ fromIntegral g
    {# set cpShapeFilter->categories #} p $ fromIntegral c
    {# set cpShapeFilter->mask #} p       $ fromIntegral m
  peek p = ShapeFilter <$> (fromIntegral <$> {# get cpShapeFilter->group #} p)
                       <*> (fromIntegral <$> {# get cpShapeFilter->categories #} p)
                       <*> (fromIntegral <$> {# get cpShapeFilter->mask #} p)

{# pointer *cpShapeFilter as ShapeFilterPtr -> ShapeFilter #}

{# fun unsafe w_cpShapeGetFilter as shapeGetFilter {`ShapePtr', alloca- `ShapeFilter' peek*} -> `()' #}

{# fun unsafe cpShapeSetFilter as shapeSetFilter {`ShapePtr', with* %`ShapeFilter'} -> `()' #}

{# fun unsafe cpShapeGetSpace as shapeGetSpace {`ShapePtr'} -> `SpacePtr' #}

{# fun unsafe cpShapeGetUserData as shapeGetUserData {`ShapePtr'} -> `DataPtr' #}

{# fun unsafe cpShapeSetUserData as shapeSetUserData {`ShapePtr', `DataPtr'} -> `()' #}

{# fun unsafe cpShapeFree as shapeFree {`ShapePtr'} -> `()' #}

{# fun unsafe w_cpShapeCacheBB as shapeCacheBB {`ShapePtr', alloca- `BB' peek*} -> `()' #}

{# fun unsafe cpCircleShapeNew as circleShapeNew {`BodyPtr', `Double', with* %`Vect'} -> `ShapePtr' #}

{# fun unsafe cpSegmentShapeNew as segmentShapeNew {`BodyPtr', with* %`Vect', with* %`Vect', `Double'} -> `ShapePtr' #}

{# fun unsafe cpSegmentShapeSetNeighbors as segmentShapeSetNeighbors {`ShapePtr', with* %`Vect', with* %`Vect'} -> `()' #}

{# fun unsafe cpPolyShapeNew as polyShapeNew {`BodyPtr', withList* `[Vect]'&, with* %`Transform', `Double'} -> `ShapePtr' #}

{# fun unsafe cpPolyShapeNewRaw as polyShapeNewRaw {`BodyPtr', withList* `[Vect]'&, `Double'} -> `ShapePtr' #}

{# fun unsafe cpBoxShapeNew as boxShapeNew {`BodyPtr', `Double', `Double', `Double'} -> `ShapePtr' #}

{# fun unsafe cpBoxShapeNew2 as boxShapeNew2 {`BodyPtr', with* %`BB', `Double'} -> `ShapePtr' #}
