-- | Description: Rigid bodies manipulations
-- Module provides access to the rigid bodies which are at the core of the physics simulation.
module Chiphunk.Low.Body
  ( BodyPtr
  , BodyType (..)
  , bodyNew
  , bodyNewKinematic
  , bodyNewStatic
  , bodyFree
  , bodyGetType
  , bodySetType
  , bodyGetMass
  , bodySetMass
  , bodyGetMoment
  , bodySetMoment
  , bodyGetPosition
  , bodySetPosition
  , bodyGetCenterOfGravity
  , bodySetCenterOfGravity
  , bodyGetVelocity
  , bodySetVelocity
  , bodyGetForce
  , bodySetForce
  , bodyGetAngle
  , bodySetAngle
  , bodyGetAngularVelocity
  , bodySetAngularVelocity
  , bodyGetTorque
  , bodySetTorque
  , bodyGetRotation
  , bodyGetSpace
  , bodyGetUserData
  , bodySetUserData
  , bodyLocalToWorld
  , bodyWorldToLocal
  , bodyGetVelocityAtWorldPoint
  , bodyGetVelocityAtLocalPoint
  , bodyApplyForceAtWorldPoint
  , bodyApplyForceAtLocalPoint
  , bodyApplyImpulseAtWorldPoint
  , bodyApplyImpulseAtLocalPoint
  , bodyIsSleeping
  , bodyActivate
  , bodySleep
  , bodyActivateStatic
  , bodySleepWithGroup
  , BodyShapeIteratorFunc
  , bodyEachShape
  , BodyConstraintIteratorFunc
  , bodyEachConstraint
  , BodyArbiterIteratorFunc
  , bodyEachArbiter
  ) where

import Chiphunk.Low.Vect
import Control.Exception.Safe
import Foreign

{# import Chiphunk.Low.Types #}

#include <chipmunk/chipmunk.h>
#include <wrapper.h>

-- | Creates body of type 'BodyTypeDynamic'.
{# fun unsafe cpBodyNew as bodyNew
  { `Double' -- ^ Mass of the body. Guessing is usually file.
  , `Double' -- ^ Moment of inertia of the body. Guessing a moment of inertia can lead to a very poor simulation
             -- so it’s recommended to use Chipmunk’s moment calculations to estimate the moment for you.
  } -> `BodyPtr' #}

-- | Create body of type 'BodyTypeKimenatic'.
{# fun unsafe cpBodyNewKinematic as bodyNewKinematic {} -> `BodyPtr' #}

-- | Create body of type 'BodyTypeStatic'.
{# fun unsafe cpBodyNewStatic as bodyNewStatic {} -> `BodyPtr' #}

-- | Be careful not to free a body before any shapes or constraints attached to it have been removed from a space.
{# fun unsafe cpBodyFree as bodyFree {`BodyPtr'} -> `()' #}

-- | Get the type of a body (dynamic, kinematic, static).
{# fun unsafe cpBodyGetType as bodyGetType {`BodyPtr'} -> `BodyType' #}

-- | Set the type of a body (dynamic, kinematic, static). When changing an body to a dynamic body,
-- the mass and moment of inertia are recalculated from the shapes added to the body.
-- Custom calculated moments of inertia are not preseved when changing types.
-- This function cannot be called directly in a collision callback.
{# fun unsafe cpBodySetType as bodySetType {`BodyPtr', `BodyType'} -> `()' #}

-- | Get mass of the body.
{# fun unsafe cpBodyGetMass as bodyGetMass {`BodyPtr'} -> `Double' #}

-- | Set mass of the body.
{# fun unsafe cpBodySetMass as bodySetMass {`BodyPtr', `Double'} -> `()' #}

-- | Get moment of inertia of the body.
{# fun unsafe cpBodyGetMoment as bodyGetMoment {`BodyPtr'} -> `Double' #}

-- | Set moment of inertial of the body. See below for function to help calculate the moment.
{# fun unsafe cpBodySetMoment as bodySetMoment {`BodyPtr', `Double'} -> `()' #}

-- | Get position of the body.
{# fun unsafe w_cpBodyGetPosition as bodyGetPosition {`BodyPtr', alloca- `Vect' peek*} -> `()' #}

-- | Set position of the body. When changing the position you may also want to call 'spaceReindexShapesForBody'
-- to update the collision detection information for the attached shapes if plan to make any queries against the space.
{# fun unsafe cpBodySetPosition as bodySetPosition {`BodyPtr', with* %`Vect'} -> `()' #}

-- | Get location of the center of gravity in body local coordinates.
-- The default value is (0, 0), meaning the center of gravity is the same as the position of the body.
{# fun unsafe w_cpBodyGetCenterOfGravity as bodyGetCenterOfGravity {`BodyPtr', alloca- `Vect' peek*} -> `()' #}

-- | Set location of the center of gravity in body local coordinates.
{# fun unsafe cpBodySetCenterOfGravity as bodySetCenterOfGravity {`BodyPtr', with* %`Vect'} -> `()' #}

-- | Get linear velocity of the center of gravity of the body.
{# fun unsafe w_cpBodyGetVelocity as bodyGetVelocity {`BodyPtr', alloca- `Vect' peek*} -> `()' #}

-- | Set linear velocity of the center of gravity of the body.
{# fun unsafe cpBodySetVelocity as bodySetVelocity {`BodyPtr', with* %`Vect'} -> `()' #}

-- | Get force applied to the center of gravity of the body.
{# fun unsafe w_cpBodyGetForce as bodyGetForce {`BodyPtr', alloca- `Vect' peek*} -> `()' #}

-- | Set force applied to the center of gravity of the body. This value is reset for every time step.
{# fun unsafe cpBodySetForce as bodySetForce {`BodyPtr', with* %`Vect'} -> `()' #}

-- | Get rotation of the body in radians.
{# fun unsafe cpBodyGetAngle as bodyGetAngle {`BodyPtr'} -> `Double' #}

-- | Set rotation of the body in radians.
-- When changing the rotation you may also want to call 'spaceReindexShapesForBody'
-- to update the collision detection information for the attached shapes if plan to make any queries against the space.
{# fun unsafe cpBodySetAngle as bodySetAngle {`BodyPtr', `Double'} -> `()' #}

-- | Get angular velocity of the body in radians per second.
{# fun unsafe cpBodyGetAngularVelocity as bodyGetAngularVelocity {`BodyPtr'} -> `Double' #}

-- | Set angular velocity of the body in radians per second.
{# fun unsafe cpBodySetAngularVelocity as bodySetAngularVelocity {`BodyPtr', `Double'} -> `()' #}

-- | Get torque applied to the body.
{# fun unsafe cpBodyGetTorque as bodyGetTorque {`BodyPtr'} -> `Double' #}

-- | Set torque applied to the body. This value is reset for every time step.
{# fun unsafe cpBodySetTorque as bodySetTorque {`BodyPtr', `Double'} -> `()' #}

-- | The rotation vector for the body. Can be used with 'vRotate' or 'vUnRotate' to perform fast rotations.
{# fun unsafe w_cpBodyGetRotation as bodyGetRotation {`BodyPtr', alloca- `Vect' peek*} -> `()' #}

-- | Get the 'SpacePtr' that body has been added to.
{# fun unsafe cpBodyGetSpace as bodyGetSpace {`BodyPtr'} -> `SpacePtr' #}

-- | Get user data pointer.
{# fun unsafe cpBodyGetUserData as bodyGetUserData {`BodyPtr'} -> `DataPtr' #}

-- | Get user data pointer. Use this pointer to get a reference to the game object that owns this body from callbacks.
{# fun unsafe cpBodySetUserData as bodySetUserData {`BodyPtr', `DataPtr'} -> `()' #}

-- | Convert from body local coordinates to world space coordinates.
{# fun unsafe w_cpBodyLocalToWorld as bodyLocalToWorld {`BodyPtr', with* %`Vect', alloca- `Vect' peek*} -> `()' #}

-- | Convert from world space coordinates to body local coordinates.
{# fun unsafe w_cpBodyWorldToLocal as bodyWorldToLocal {`BodyPtr', with* %`Vect', alloca- `Vect' peek*} -> `()' #}

-- | Get the absolute velocity of the rigid body at the given world point.
{# fun unsafe w_cpBodyGetVelocityAtWorldPoint as bodyGetVelocityAtWorldPoint
  {`BodyPtr', with* %`Vect', alloca- `Vect' peek*} -> `()' #}

-- | Get the absolute velocity of the rigid body at the given body local point.
{# fun unsafe w_cpBodyGetVelocityAtLocalPoint as bodyGetVelocityAtLocalPoint
  {`BodyPtr', with* %`Vect', alloca- `Vect' peek*} -> `()' #}

-- | Add the @force@ to @body@ as if applied from the world @point@.
{# fun unsafe cpBodyApplyForceAtWorldPoint as bodyApplyForceAtWorldPoint
  { `BodyPtr'     -- ^ body
  , with* %`Vect' -- ^ force
  , with* %`Vect' -- ^ point
  } -> `()' #}

-- | Add the local @force@ to @body@ as if applied from the body local @point@.
{# fun unsafe cpBodyApplyForceAtLocalPoint as bodyApplyForceAtLocalPoint
  { `BodyPtr'     -- ^ body
  , with* %`Vect' -- ^ force
  , with* %`Vect' -- ^ point
  } -> `()' #}

-- | Add the @impulse@ to @body@ as if applied from the world @ point@.
{# fun unsafe cpBodyApplyImpulseAtWorldPoint as bodyApplyImpulseAtWorldPoint
  { `BodyPtr'     -- ^ body
  , with* %`Vect' -- ^ impulse
  , with* %`Vect' -- ^ point
  } -> `()' #}

-- | Add the local @impulse@ to @body@ as if applied from the body local @point@.
{# fun unsafe cpBodyApplyImpulseAtLocalPoint as bodyApplyImpulseAtLocalPoint
  { `BodyPtr'     -- ^ body
  , with* %`Vect' -- ^ impulse
  , with* %`Vect' -- ^ point
  } -> `()' #}

-- | Returns true if body is sleeping.
{# fun unsafe cpBodyIsSleeping as bodyIsSleeping {`BodyPtr'} -> `Bool' #}

-- | Reset the idle timer on a body. If it was sleeping, wake it and any other bodies it was touching.
{# fun unsafe cpBodyActivate as bodyActivate {`BodyPtr'} -> `()' #}

-- | Forces a body to fall asleep immediately even if it’s in midair. Cannot be called from a callback.
{# fun unsafe cpBodySleep as bodySleep {`BodyPtr'} -> `()' #}

-- | Activates all bodies touching @body@.
-- If @filter@ is not 'nullPtr', then only bodies touching through @filter@ will be awoken.
{# fun unsafe cpBodyActivateStatic as bodyActivateStatic
  { `BodyPtr'  -- ^ body
  , `ShapePtr' -- ^ filter
  } -> `()' #}

-- | When objects in Chipmunk sleep, they sleep as a group of all objects that are touching or jointed together.
-- When an object is woken up, all of the objects in its group are woken up.
-- 'bodySleepWithGroup' allows you group sleeping objects together.
-- It acts identically to 'bodySleep' if you pass 'nullPtr' as @group@ by starting a new group.
-- If you pass a sleeping body for @group@, @body@ will be awoken when group is awoken.
-- You can use this to initialize levels and start stacks of objects in a pre-sleeping state.
{# fun unsafe cpBodySleepWithGroup as bodySleepWithGroup
  { `BodyPtr' -- ^ body
  , `BodyPtr' -- ^ group
  } -> `()' #}

-- | Type of callback which can be used to iterate all 'ShapePtr's in a 'BodyPtr'.
type BodyShapeIteratorFunc = BodyPtr -> ShapePtr -> Ptr () -> IO ()

foreign import ccall unsafe "wrapper"
  mkBodyShapeIteratorFunc :: BodyShapeIteratorFunc -> IO (FunPtr BodyShapeIteratorFunc)

-- | Call @func@ once for each shape that is attached to @body@ and added to a space.
-- @data@ is passed along as a context value. It is safe to remove shapes using these callbacks.
{# fun cpBodyEachShape as bodyEachShape
  { `BodyPtr'                             -- ^ body
  , withIterator* `BodyShapeIteratorFunc' -- ^ func
  , `Ptr ()'                              -- ^ data
  } -> `()' #}
  where
    withIterator i = mkBodyShapeIteratorFunc i `bracket` freeHaskellFunPtr

-- | Type of callback which can be used to iterate all 'ConstraintPtr's in a 'BodyPtr'.
type BodyConstraintIteratorFunc = BodyPtr -> ConstraintPtr -> Ptr () -> IO ()

foreign import ccall unsafe "wrapper"
  mkBodyConstraintIteratorFunc :: BodyConstraintIteratorFunc -> IO (FunPtr BodyConstraintIteratorFunc)

-- | Call @func@ once for each constraint that is attached to @body@ and added to a space.
-- @data@ is passed along as a context value. It is safe to remove constraints using thes callbacks.
{# fun cpBodyEachConstraint as bodyEachConstraint
  { `BodyPtr'                                  -- ^ body
  , withIterator* `BodyConstraintIteratorFunc' -- ^ func
  , `Ptr ()'                                   -- ^ data
  } -> `()' #}
  where
    withIterator i = mkBodyConstraintIteratorFunc i `bracket` freeHaskellFunPtr

-- | Type of callback which can be used to iterate all 'ArbiterPtr's in a 'BodyPtr'.
type BodyArbiterIteratorFunc = BodyPtr -> ArbiterPtr -> Ptr () -> IO ()

foreign import ccall unsafe "wrapper"
  mkBodyArbiterIteratorFunc :: BodyArbiterIteratorFunc -> IO (FunPtr BodyArbiterIteratorFunc)

-- | This one is more interesting. Calls @func@ once for each collision pair that @body@ is involved in.
-- Calling 'arbiterGetBodies'/'arbiterGetShapes' will return the body or shape for body as the first argument.
-- You can use this to check all sorts of collision information for a body like if it’s touching the ground,
-- another particular object, how much collision force is being applied to an object, etc.
--
-- Sensor shapes and arbiters that have been rejected by a collision handler callback or 'arbiterIgnore'
-- are not tracked by the contact graph.
{# fun cpBodyEachArbiter as bodyEachArbiter
  { `BodyPtr'                               -- ^ body
  , withIterator* `BodyArbiterIteratorFunc' -- ^ func
  , `Ptr ()'                                -- ^ data
  } -> `()' #}
  where
    withIterator i = mkBodyArbiterIteratorFunc i `bracket` freeHaskellFunPtr
