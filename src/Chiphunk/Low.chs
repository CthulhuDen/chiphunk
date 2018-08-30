{-# LANGUAGE ForeignFunctionInterface #-}

module Chiphunk.Low
  ( module Chiphunk.Low
  , module Chiphunk.Low.Types
  , module Chiphunk.Low.Math
  , module Chiphunk.Low.Helper
  , module Chiphunk.Low.Body
  , module Chiphunk.Low.Vect
  , nullPtr
  ) where

import Foreign
import System.IO.Unsafe

import Chiphunk.Low.Math
import Chiphunk.Low.Helper
import Chiphunk.Low.Vect
import Chiphunk.Low.BB
import Chiphunk.Low.Body
{# import Chiphunk.Low.Types #}

#include <chipmunk/chipmunk.h>
#include <wrapper.h>

{# fun unsafe cpSpaceNew as spaceNew {} -> `SpacePtr' #}

{# fun unsafe cpSpaceSetGravity as spaceSetGravity {`SpacePtr', with* %`Vect'} -> `()' #}

{# fun unsafe w_cpSpaceGetGravity as spaceGetGravity {`SpacePtr', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpSpaceGetStaticBody as spaceGetStaticBody {`SpacePtr'} -> `BodyPtr' #}

{# fun unsafe cpSegmentShapeNew as segmentShapeNew {`BodyPtr', with* %`Vect', with* %`Vect', `Double'} -> `ShapePtr' #}

{# fun unsafe cpCircleShapeNew as circleShapeNew {`BodyPtr', `Double', with* %`Vect'} -> `ShapePtr' #}

{# fun unsafe cpShapeSetFriction as shapeSetFriction {`ShapePtr', `Double'} -> `()' #}

{# fun unsafe cpSpaceAddShape as spaceAddShape {`SpacePtr', `ShapePtr'} -> `()' #}

{# fun unsafe cpSpaceAddBody as spaceAddBody {`SpacePtr', `BodyPtr'} -> `()' #}

{# fun unsafe cpSpaceStep as spaceStep {`SpacePtr', `Double'} -> `()' #}

{# fun unsafe cpShapeFree as shapeFree {`ShapePtr'} -> `()' #}

{# fun unsafe cpSpaceFree as spaceFree {`SpacePtr'} -> `()' #}
