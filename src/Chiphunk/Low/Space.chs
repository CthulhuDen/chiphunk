-- | Description: Manipulate space
-- Module defined utilities for manipulating spaces.
module Chiphunk.Low.Space
  ( SpacePtr
  , spaceGetIterations
  , spaceSetIterations
  , spaceGetGravity
  , spaceSetGravity
  , spaceGetDamping
  , spaceSetDamping
  , spaceGetIdleSpeedThreshold
  , spaceSetIdleSpeedThreshold
  , spaceGetSleepTimeThreshold
  , spaceSetSleepTimeThreshold
  , spaceGetCollisionSlop
  , spaceSetCollisionSlop
  , spaceGetCollisionBias
  , spaceSetCollisionBias
  , spaceGetCollisionPersistence
  , spaceSetCollisionPersistence
  , spaceGetCurrentTimeStep
  , spaceIsLocked
  , spaceGetUserData
  , spaceSetUserData
  , spaceGetStaticBody
  , spaceNew
  , spaceFree
  , spaceAddShape
  , spaceAddBody
  , spaceAddConstraint
  , spaceRemoveShape
  , spaceRemoveBody
  , spaceRemoveConstraint
  , spaceContainsShape
  , spaceContainsBody
  , spaceContainsConstraint
  , spaceReindexShape
  , spaceReindexShapesForBody
  , spaceReindexStatic
  , SpaceBodyIteratorFunc
  , spaceEachBody
  , SpaceShapeIteratorFunc
  , spaceEachShape
  , SpaceConstraintIteratorFunc
  , spaceEachConstraint
  , spaceStep
  ) where

import Control.Exception.Safe
import Foreign

import Chiphunk.Low.Vect
{# import Chiphunk.Low.Types #}

#include <chipmunk/chipmunk.h>
#include <wrapper.h>

-- | Get current iterations. Defaults to 10.
{# fun unsafe cpSpaceGetIterations as spaceGetIterations {`SpacePtr'} -> `Int' #}

-- | Set current iterations.
{# fun unsafe cpSpaceSetIterations as spaceSetIterations {`SpacePtr', `Int'} -> `()' #}

-- | Get global gravity applied to the space. Defaults to 'vZero'.
{# fun unsafe w_cpSpaceGetGravity as spaceGetGravity {`SpacePtr', alloca- `Vect' peek*} -> `()' #}

-- | Set global gravity applied to the space. Can be overridden on a per body basis
-- by writing custom integration functions. Changing the gravity will activate all sleeping bodies in the space.
{# fun unsafe cpSpaceSetGravity as spaceSetGravity {`SpacePtr', with* %`Vect'} -> `()' #}

-- | Get amount of simple damping to apply to the space. Defaults to 1.
{# fun unsafe cpSpaceGetDamping as spaceGetDamping {`SpacePtr'} -> `Double' #}

-- | Set amount of simple damping to apply to the space. Like gravity, it can be overridden on a per body basis.
{# fun unsafe cpSpaceSetDamping as spaceSetDamping {`SpacePtr', `Double'} -> `()' #}

-- | Get speed threshold for a body to be considered idle.
-- The default value of 0 means the space estimates a good threshold based on gravity.
{# fun unsafe cpSpaceGetIdleSpeedThreshold as spaceGetIdleSpeedThreshold {`SpacePtr'} -> `Double' #}

-- | Set speed threshold for a body to be considered idle.
{# fun unsafe cpSpaceSetIdleSpeedThreshold as spaceSetIdleSpeedThreshold {`SpacePtr', `Double'} -> `()' #}

-- | Get time a group of bodies must remain idle in order to fall asleep.
-- The default value of INFINITY disables the sleeping feature.
{# fun unsafe cpSpaceGetSleepTimeThreshold as spaceGetSleepTimeThreshold {`SpacePtr'} -> `Double' #}

-- | Set time a group of bodies must remain idle in order to fall asleep.
{# fun unsafe cpSpaceSetSleepTimeThreshold as spaceSetSleepTimeThreshold {`SpacePtr', `Double'} -> `()' #}

-- | Get amount of overlap between shapes that is allowed. It defaults to 0.1.
{# fun unsafe cpSpaceGetCollisionSlop as spaceGetCollisionSlop {`SpacePtr'} -> `Double' #}

-- | Set amount of overlap between shapes that is allowed.
-- To improve stability, set this as high as you can without noticable overlapping.
{# fun unsafe cpSpaceSetCollisionSlop as spaceSetCollisionSlop {`SpacePtr', `Double'} -> `()' #}

-- | Get collision bias. The default value is calculated as cpfpow(1.0f - 0.1f, 60.0f)
-- meaning that Chipmunk attempts to correct 10% of error ever 1/60th of a second.
{# fun unsafe cpSpaceGetCollisionBias as spaceGetCollisionBias {`SpacePtr'} -> `Double' #}

-- | Set collision bias. Valid values are in the range from 0 to 1, but using 0 is not recommended
-- for stability reasons.
{# fun unsafe cpSpaceSetCollisionBias as spaceSetCollisionBias {`SpacePtr', `Double'} -> `()' #}

-- | Get the number of frames the space keeps collision solutions around for. This defaults to 3
{# fun unsafe cpSpaceGetCollisionPersistence as spaceGetCollisionPersistence {`SpacePtr'} -> `Word32' #}

-- | Set the number of frames the space keeps collision solutions around for.
-- Very very very few games will need to change this value.
{# fun unsafe cpSpaceSetCollisionPersistence as spaceSetCollisionPersistence {`SpacePtr', `Word32'} -> `()' #}

-- | Retrieves the current (if you are in a callback from 'spaceStep')
-- or most recent (outside of a 'spaceStep' call) timestep.
{# fun unsafe cpSpaceGetCurrentTimeStep as spaceGetCurrentTimeStep {`SpacePtr'} -> `Double' #}

-- | Returns true when you cannot add/remove objects from the space.
-- In particular, spaces are locked when in a collision callback.
-- Instead, run your code in a post-step callback instead.
{# fun unsafe cpSpaceIsLocked as spaceIsLocked {`SpacePtr'} -> `Bool' #}

-- | Get the user definable data pointer.
{# fun unsafe cpSpaceGetUserData as spaceGetUserData {`SpacePtr'} -> `DataPtr' #}

-- | Set a user definable data pointer.
-- It is often useful to point this at the gamestate object or scene management object that owns the space.
{# fun unsafe cpSpaceSetUserData as spaceSetUserData {`SpacePtr', `DataPtr'} -> `()' #}

-- | A dedicated static body for the space.
-- You don’t have to use it, but because its memory is managed automatically with the space its very convenient.
-- You can set its user data pointer to something helpful if you want for callbacks.
{# fun unsafe cpSpaceGetStaticBody as spaceGetStaticBody {`SpacePtr'} -> `BodyPtr' #}

-- | Standard Chipmunk allocation function.
{# fun unsafe cpSpaceNew as spaceNew {} -> `SpacePtr' #}

-- | Standard Chipmunk deallocation function.
{# fun unsafe cpSpaceFree as spaceFree {`SpacePtr'} -> `()' #}

-- | Add shape to the space.
{# fun unsafe cpSpaceAddShape as spaceAddShape {`SpacePtr', `ShapePtr'} -> `()' #}

-- | Add body to the space.
{# fun unsafe cpSpaceAddBody as spaceAddBody {`SpacePtr', `BodyPtr'} -> `()' #}

-- | Add constraint to the space.
{# fun unsafe cpSpaceAddConstraint as spaceAddConstraint {`SpacePtr', `ConstraintPtr'} -> `()' #}

-- | Remove shape from the space.
{# fun unsafe cpSpaceRemoveShape as spaceRemoveShape {`SpacePtr', `ShapePtr'} -> `()' #}

-- | Remove body from the space.
{# fun unsafe cpSpaceRemoveBody as spaceRemoveBody {`SpacePtr', `BodyPtr'} -> `()' #}

-- | Remove constraint from the space.
{# fun unsafe cpSpaceRemoveConstraint as spaceRemoveConstraint {`SpacePtr', `ConstraintPtr'} -> `()' #}

-- | Check if shape is attached to the space.
{# fun unsafe cpSpaceContainsShape as spaceContainsShape {`SpacePtr', `ShapePtr'} -> `Bool' #}

-- | Check if body is attached to the space.
{# fun unsafe cpSpaceContainsBody as spaceContainsBody {`SpacePtr', `BodyPtr'} -> `Bool' #}

-- | Check if constraint is attached to the space.
{# fun unsafe cpSpaceContainsConstraint as spaceContainsConstraint {`SpacePtr', `ConstraintPtr'} -> `Bool' #}

-- | Reindex a specific shape.
{# fun unsafe cpSpaceReindexShape as spaceReindexShape {`SpacePtr', `ShapePtr'} -> `()' #}

-- | Reindex all the shapes for a certain body.
{# fun unsafe cpSpaceReindexShapesForBody as spaceReindexShapesForBody {`SpacePtr', `BodyPtr'} -> `()' #}

-- | Reindex all static shapes. Generally updating only the shapes that changed is faster.
{# fun unsafe cpSpaceReindexStatic as spaceReindexStatic {`SpacePtr'} -> `()' #}

-- | Type of callback which can be used to iterate all 'BodyPtr's in a 'SpacePtr'.
type SpaceBodyIteratorFunc = BodyPtr -> Ptr () -> IO ()

foreign import ccall unsafe "wrapper"
  mkSpaceBodyIteratorFunc :: SpaceBodyIteratorFunc -> IO (FunPtr SpaceBodyIteratorFunc)

-- | Call @func@ for each body in the @space@ also passing along your @data@ pointer.
-- Sleeping bodies are included, but static and kinematic bodies are not as they aren’t added to the space.
{# fun cpSpaceEachBody as spaceEachBody
  { `SpacePtr'                            -- ^ space
  , withIterator* `SpaceBodyIteratorFunc' -- ^ func
  , `Ptr ()'                              -- ^ data
  } -> `()' #}
  where
    withIterator i = mkSpaceBodyIteratorFunc i `bracket` freeHaskellFunPtr

-- | Type of callback which can be used to iterate all 'ShapePtr's in a 'SpacePtr'.
type SpaceShapeIteratorFunc = ShapePtr -> Ptr () -> IO ()

foreign import ccall unsafe "wrapper"
  mkSpaceShapeIteratorFunc :: SpaceShapeIteratorFunc -> IO (FunPtr SpaceShapeIteratorFunc)

-- | Call @func@ for each shape in the @space@ also passing along your @data@ pointer.
-- Sleeping and static shapes are included.
{# fun cpSpaceEachShape as spaceEachShape
  { `SpacePtr'                             -- ^ space
  , withIterator* `SpaceShapeIteratorFunc' -- ^ func
  , `Ptr ()'                               -- ^ data
  } -> `()' #}
  where
    withIterator i = mkSpaceShapeIteratorFunc i `bracket` freeHaskellFunPtr

-- | Type of callback which can be used to iterate all 'ConstraintPtr's in a 'SpacePtr'.
type SpaceConstraintIteratorFunc = ConstraintPtr -> Ptr () -> IO ()

foreign import ccall unsafe "wrapper"
  mkSpaceConstraintIteratorFunc :: SpaceConstraintIteratorFunc -> IO (FunPtr SpaceConstraintIteratorFunc)

-- | Call func for each constraint in the space also passing along your data pointer.
{# fun cpSpaceEachConstraint as spaceEachConstraint
  { `SpacePtr'                                  -- ^ space
  , withIterator* `SpaceConstraintIteratorFunc' -- ^ func
  , `Ptr ()'                                    -- ^ data
  } -> `()' #}
  where
    withIterator i = mkSpaceConstraintIteratorFunc i `bracket` freeHaskellFunPtr

-- | Update the space for the given time step. Using a fixed time step is highly recommended.
-- Doing so can greatly increase the quality of the simulation.
-- The easiest way to do constant timesteps is to simple step forward by 1/60th of a second
-- (or whatever your target framerate is) for each frame regardless of how long it took to render.
-- This works fine for many games, but a better way to do it is to separate your physics timestep and rendering.
{# fun unsafe cpSpaceStep as spaceStep {`SpacePtr', `Double'} -> `()' #}
