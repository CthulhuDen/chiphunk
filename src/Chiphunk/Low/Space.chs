-- | Description: Manipulate space
-- Module defined utilities for manipulating spaces.
module Chiphunk.Low.Space
  ( Space
  , spaceIterations
  , spaceGravity
  , spaceDamping
  , spaceIdleSpeedThreshold
  , spaceSleepTimeThreshold
  , spaceCollisionSlop
  , spaceCollisionBias
  , spaceCollisionPersistence
  , spaceCurrentTimeStep
  , spaceIsLocked
  , spaceUserData
  , spaceStaticBody
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
import Data.StateVar
import Foreign

import Chiphunk.Low.Vect
{# import Chiphunk.Low.Types #}

#include <chipmunk/chipmunk.h>
#include <wrapper.h>

{# fun unsafe cpSpaceGetIterations {`Space'} -> `Int' #}

{# fun unsafe cpSpaceSetIterations {`Space', `Int'} -> `()' #}

-- | Iterations allow you to control the accuracy of the solver.
-- Defaults to 10. See above for more information.
spaceIterations :: Space -> StateVar Int
spaceIterations = mkStateVar cpSpaceGetIterations cpSpaceSetIterations

{# fun unsafe w_cpSpaceGetGravity {`Space', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpSpaceSetGravity {`Space', with* %`Vect'} -> `()' #}

-- | Global gravity applied to the space. Defaults to 'vZero'.
-- Can be overridden on a per body basis by writing custom integration functions.
-- Changing the gravity will activate all sleeping bodies in the space.
spaceGravity :: Space -> StateVar Vect
spaceGravity = mkStateVar w_cpSpaceGetGravity cpSpaceSetGravity

{# fun unsafe cpSpaceGetDamping {`Space'} -> `Double' #}

{# fun unsafe cpSpaceSetDamping {`Space', `Double'} -> `()' #}

-- | Amount of simple damping to apply to the space.
-- A value of 0.9 means that each body will lose 10% of its velocity per second.
-- Defaults to 1. Like gravity, it can be overridden on a per body basis.
spaceDamping :: Space -> StateVar Double
spaceDamping = mkStateVar cpSpaceGetDamping cpSpaceSetDamping

{# fun unsafe cpSpaceGetIdleSpeedThreshold {`Space'} -> `Double' #}

{# fun unsafe cpSpaceSetIdleSpeedThreshold {`Space', `Double'} -> `()' #}

-- | Speed threshold for a body to be considered idle.
-- The default value of 0 means the space estimates a good threshold based on gravity.
spaceIdleSpeedThreshold :: Space -> StateVar Double
spaceIdleSpeedThreshold = mkStateVar cpSpaceGetIdleSpeedThreshold cpSpaceSetIdleSpeedThreshold

{# fun unsafe cpSpaceGetSleepTimeThreshold {`Space'} -> `Double' #}

{# fun unsafe cpSpaceSetSleepTimeThreshold {`Space', `Double'} -> `()' #}

-- | Time a group of bodies must remain idle in order to fall asleep.
-- The default value of INFINITY disables the sleeping feature.
spaceSleepTimeThreshold :: Space -> StateVar Double
spaceSleepTimeThreshold = mkStateVar cpSpaceGetSleepTimeThreshold cpSpaceSetSleepTimeThreshold

{# fun unsafe cpSpaceGetCollisionSlop {`Space'} -> `Double' #}

{# fun unsafe cpSpaceSetCollisionSlop {`Space', `Double'} -> `()' #}

-- | Amount of overlap between shapes that is allowed.
-- To improve stability, set this as high as you can without noticable overlapping.
-- It defaults to @0.1@.
spaceCollisionSlop :: Space -> StateVar Double
spaceCollisionSlop = mkStateVar cpSpaceGetCollisionSlop cpSpaceSetCollisionSlop

{# fun unsafe cpSpaceGetCollisionBias {`Space'} -> `Double' #}

{# fun unsafe cpSpaceSetCollisionBias {`Space', `Double'} -> `()' #}

-- | Chipmunk allows fast moving objects to overlap, then fixes the overlap over time.
-- Overlapping objects are unavoidable even if swept collisions are supported,
-- and this is an efficient and stable way to deal with overlapping objects.
-- The bias value controls what percentage of overlap remains unfixed
-- after a second and defaults to ~0.2%.
--
-- Valid values are in the range from 0 to 1,
-- but using 0 is not recommended for stability reasons.
--
-- The default value is calculated as @(1.0 - 0.1) ^ 60@
-- meaning that Chipmunk attempts to correct 10% of error ever 1/60th of a second.
--
-- __Note__: Very very few games will need to change this value.
spaceCollisionBias :: Space -> StateVar Double
spaceCollisionBias = mkStateVar cpSpaceGetCollisionBias cpSpaceSetCollisionBias

{# fun unsafe cpSpaceGetCollisionPersistence {`Space'} -> `Word32' #}

{# fun unsafe cpSpaceSetCollisionPersistence {`Space', `Word32'} -> `()' #}

-- | The number of frames the space keeps collision solutions around for.
-- Helps prevent jittering contacts from getting worse.
-- This defaults to 3 and very very very few games will need to change this value.
spaceCollisionPersistence :: Space -> StateVar Word32
spaceCollisionPersistence = mkStateVar cpSpaceGetCollisionPersistence cpSpaceSetCollisionPersistence

{# fun unsafe cpSpaceGetCurrentTimeStep {`Space'} -> `Double' #}

-- | The current (if you are in a callback from 'spaceStep')
-- or most recent (outside of a 'spaceStep' call) timestep.
spaceCurrentTimeStep :: Space -> GettableStateVar Double
spaceCurrentTimeStep = makeGettableStateVar . cpSpaceGetCurrentTimeStep

-- | Returns true when you cannot add/remove objects from the space.
-- In particular, spaces are locked when in a collision callback.
-- Instead, run your code in a post-step callback instead.
{# fun unsafe cpSpaceIsLocked as spaceIsLocked {`Space'} -> `Bool' #}

{# fun unsafe cpSpaceGetUserData {`Space'} -> `DataPtr' #}

{# fun unsafe cpSpaceSetUserData {`Space', `DataPtr'} -> `()' #}

-- | A user definable data pointer.
-- It is often useful to point this at the gamestate object
-- or scene management object that owns the space.
spaceUserData :: Space -> StateVar DataPtr
spaceUserData = mkStateVar cpSpaceGetUserData cpSpaceSetUserData

{# fun unsafe cpSpaceGetStaticBody {`Space'} -> `Body' #}

-- | A dedicated static body for the space.
-- You don’t have to use it,
-- but because its memory is managed automatically with the space its very convenient.
-- You can set its user data pointer to something helpful if you want for callbacks.
spaceStaticBody :: Space -> GettableStateVar Body
spaceStaticBody = makeGettableStateVar . cpSpaceGetStaticBody

-- | Standard Chipmunk allocation function.
{# fun unsafe cpSpaceNew as spaceNew {} -> `Space' #}

-- | Standard Chipmunk deallocation function.
{# fun cpSpaceFree as spaceFree {`Space'} -> `()' #}
-- no "unsafe" qualifier because I think it may trigger separte callbacks

-- | Add shape to the space.
{# fun unsafe cpSpaceAddShape as spaceAddShape {`Space', `Shape'} -> `()' #}

-- | Add body to the space.
{# fun unsafe cpSpaceAddBody as spaceAddBody {`Space', `Body'} -> `()' #}

-- | Add constraint to the space.
{# fun unsafe cpSpaceAddConstraint as spaceAddConstraint {`Space', `Constraint'} -> `()' #}

-- | Remove shape from the space.
{# fun cpSpaceRemoveShape as spaceRemoveShape {`Space', `Shape'} -> `()' #}
-- no "unsafe" qualifier because I think it may trigger separte callbacks

-- | Remove body from the space.
{# fun cpSpaceRemoveBody as spaceRemoveBody {`Space', `Body'} -> `()' #}
-- no "unsafe" qualifier because I think it may trigger separte callbacks

-- | Remove constraint from the space.
{# fun cpSpaceRemoveConstraint as spaceRemoveConstraint {`Space', `Constraint'} -> `()' #}
-- no "unsafe" qualifier because I think it may trigger separte callbacks

-- | Check if shape is attached to the space.
{# fun unsafe cpSpaceContainsShape as spaceContainsShape {`Space', `Shape'} -> `Bool' #}

-- | Check if body is attached to the space.
{# fun unsafe cpSpaceContainsBody as spaceContainsBody {`Space', `Body'} -> `Bool' #}

-- | Check if constraint is attached to the space.
{# fun unsafe cpSpaceContainsConstraint as spaceContainsConstraint {`Space', `Constraint'} -> `Bool' #}

-- | Reindex a specific shape.
{# fun unsafe cpSpaceReindexShape as spaceReindexShape {`Space', `Shape'} -> `()' #}

-- | Reindex all the shapes for a certain body.
{# fun unsafe cpSpaceReindexShapesForBody as spaceReindexShapesForBody {`Space', `Body'} -> `()' #}

-- | Reindex all static shapes. Generally updating only the shapes that changed is faster.
{# fun unsafe cpSpaceReindexStatic as spaceReindexStatic {`Space'} -> `()' #}

-- | Type of callback which can be used to iterate all 'Body's in a 'Space'.
type SpaceBodyIteratorFunc = Body -> Ptr () -> IO ()

foreign import ccall unsafe "wrapper"
  mkSpaceBodyIteratorFunc :: SpaceBodyIteratorFunc -> IO (FunPtr SpaceBodyIteratorFunc)

-- | Call @func@ for each body in the @space@ also passing along your @data@ pointer.
-- Sleeping bodies are included, but static and kinematic bodies are not as they aren’t added to the space.
{# fun cpSpaceEachBody as spaceEachBody
  { `Space'                               -- ^ space
  , withIterator* `SpaceBodyIteratorFunc' -- ^ func
  , `Ptr ()'                              -- ^ data
  } -> `()' #}
  where
    withIterator i = mkSpaceBodyIteratorFunc i `bracket` freeHaskellFunPtr

-- | Type of callback which can be used to iterate all 'Shape's in a 'Space'.
type SpaceShapeIteratorFunc = Shape -> Ptr () -> IO ()

foreign import ccall unsafe "wrapper"
  mkSpaceShapeIteratorFunc :: SpaceShapeIteratorFunc -> IO (FunPtr SpaceShapeIteratorFunc)

-- | Call @func@ for each shape in the @space@ also passing along your @data@ pointer.
-- Sleeping and static shapes are included.
{# fun cpSpaceEachShape as spaceEachShape
  { `Space'                                -- ^ space
  , withIterator* `SpaceShapeIteratorFunc' -- ^ func
  , `Ptr ()'                               -- ^ data
  } -> `()' #}
  where
    withIterator i = mkSpaceShapeIteratorFunc i `bracket` freeHaskellFunPtr

-- | Type of callback which can be used to iterate all 'Constraint's in a 'Space'.
type SpaceConstraintIteratorFunc = Constraint -> Ptr () -> IO ()

foreign import ccall unsafe "wrapper"
  mkSpaceConstraintIteratorFunc :: SpaceConstraintIteratorFunc -> IO (FunPtr SpaceConstraintIteratorFunc)

-- | Call func for each constraint in the space also passing along your data pointer.
{# fun cpSpaceEachConstraint as spaceEachConstraint
  { `Space'                                     -- ^ space
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
{# fun cpSpaceStep as spaceStep {`Space', `Double'} -> `()' #}
