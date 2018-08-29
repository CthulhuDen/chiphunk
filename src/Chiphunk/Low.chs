{-# LANGUAGE ForeignFunctionInterface #-}

module Chiphunk.Low
  ( module Chiphunk.Low
  , module Chiphunk.Low.Types
  , module Chiphunk.Low.Vect
  ) where

import Foreign
import System.IO.Unsafe

import Chiphunk.Low.Vect
{# import Chiphunk.Low.Types #}

#include <chipmunk/chipmunk.h>
#include <wrapper.h>

{# fun unsafe cpSpaceNew as spaceNew {} -> `SpacePtr' #}

{# fun unsafe cpSpaceSetGravity as spaceSetGravity {`SpacePtr', with* %`Vect'} -> `()' #}

{# fun unsafe w_cpSpaceGetGravity as spaceGetGravity {`SpacePtr', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpSpaceGetStaticBody as spaceGetStaticBody {`SpacePtr'} -> `BodyPtr' #}

{# fun unsafe cpBodyNew as bodyNew {`Double', `Double'} -> `BodyPtr' #}

{# fun unsafe cpSegmentShapeNew as segmentShapeNew {`BodyPtr', with* %`Vect', with* %`Vect', `Double'} -> `ShapePtr' #}

{# fun unsafe cpCircleShapeNew as circleShapeNew {`BodyPtr', `Double', with* %`Vect'} -> `ShapePtr' #}

{# fun pure unsafe cpMomentForCircle as momentForCircle {`Double', `Double', `Double', with* %`Vect'} -> `Double' #}

{# fun unsafe cpShapeSetFriction as shapeSetFriction {`ShapePtr', `Double'} -> `()' #}

{# fun unsafe cpSpaceAddShape as spaceAddShape {`SpacePtr', `ShapePtr'} -> `()' #}

{# fun unsafe cpSpaceAddBody as spaceAddBody {`SpacePtr', `BodyPtr'} -> `()' #}

{# fun unsafe cpBodySetPosition as bodySetPosition {`BodyPtr', with* %`Vect'} -> `()' #}

{# fun unsafe w_cpBodyGetPosition as bodyGetPosition {`BodyPtr', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe w_cpBodyGetVelocity as bodyGetVelocity {`BodyPtr', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpSpaceStep as spaceStep {`SpacePtr', `Double'} -> `()' #}

{# fun unsafe cpBodyFree as bodyFree {`BodyPtr'} -> `()' #}

{# fun unsafe cpShapeFree as shapeFree {`ShapePtr'} -> `()' #}

{# fun unsafe cpSpaceFree as spaceFree {`SpacePtr'} -> `()' #}
