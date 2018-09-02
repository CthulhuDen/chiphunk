-- | Description: Dealing with joints/constraints
-- Module defines utilities for operations with constraints.
module Chiphunk.Low.Constraint
  ( ConstraintPtr
  , constraintGetBodyA
  , constraintGetBodyB
  , constraintGetMaxForce
  , constraintSetMaxForce
  , constraintGetErrorBias
  , constraintSetErrorBias
  , constraintGetMaxBias
  , constraintSetMaxBias
  , constraintGetSpace
  , constraintGetCollideBodies
  , constraintSetCollideBodies
  , constraintGetUserData
  , constraintSetUserData
  , constraintGetImpulse
  , constraintFree
  , pinJointNew
  , pinJointGetAnchorA
  , pinJointSetAnchorA
  , pinJointGetAnchorB
  , pinJointSetAnchorB
  , pinJointGetDist
  , pinJointSetDist
  , slideJointNew
  , slideJointGetAnchorA
  , slideJointSetAnchorA
  , slideJointGetAnchorB
  , slideJointSetAnchorB
  , slideJointGetMin
  , slideJointSetMin
  , slideJointGetMax
  , slideJointSetMax
  , pivotJointNew
  , pivotJointNew2
  , pivotJointGetAnchorA
  , pivotJointSetAnchorA
  , pivotJointGetAnchorB
  , pivotJointSetAnchorB
  , grooveJointNew
  , grooveJointGetGrooveA
  , grooveJointSetGrooveA
  , grooveJointGetGrooveB
  , grooveJointSetGrooveB
  , grooveJointGetAnchorB
  , grooveJointSetAnchorB
  , dampedSpringNew
  , dampedSpringGetAnchorA
  , dampedSpringSetAnchorA
  , dampedSpringGetAnchorB
  , dampedSpringSetAnchorB
  , dampedSpringGetRestLength
  , dampedSpringSetRestLength
  , dampedSpringGetStiffness
  , dampedSpringSetStiffness
  , dampedSpringGetDamping
  , dampedSpringSetDamping
  , dampedRotarySpringNew
  , dampedRotarySpringGetRestAngle
  , dampedRotarySpringSetRestAngle
  , dampedRotarySpringGetStiffness
  , dampedRotarySpringSetStiffness
  , dampedRotarySpringGetDamping
  , dampedRotarySpringSetDamping
  , rotaryLimitJointNew
  , rotaryLimitJointGetMin
  , rotaryLimitJointSetMin
  , rotaryLimitJointGetMax
  , rotaryLimitJointSetMax
  , ratchetJointNew
  , ratchetJointGetAngle
  , ratchetJointSetAngle
  , ratchetJointGetPhase
  , ratchetJointSetPhase
  , ratchetJointGetRatchet
  , ratchetJointSetRatchet
  , gearJointNew
  , gearJointGetPhase
  , gearJointSetPhase
  , gearJointGetRatio
  , gearJointSetRatio
  , simpleMotorNew
  , simpleMotorGetRate
  , simpleMotorSetRate
  ) where

import Foreign

{# import Chiphunk.Low.Types #}

#include <chipmunk/chipmunk.h>
#include <wrapper.h>

-- | Get the first body constraint is attached to
{# fun unsafe cpConstraintGetBodyA as constraintGetBodyA {`ConstraintPtr'} -> `BodyPtr' #}

-- | Get the second body constraint is attached to
{# fun unsafe cpConstraintGetBodyB as constraintGetBodyB {`ConstraintPtr'} -> `BodyPtr' #}

-- | Get the maximum force that the constraint can use to act on the two bodies. Defaults to INFINITY.
{# fun unsafe cpConstraintGetMaxForce as constraintGetMaxForce {`ConstraintPtr'} -> `Double' #}

-- | Set the maximum force that the constraint can use to act on the two bodies.
{# fun unsafe cpConstraintSetMaxForce as constraintSetMaxForce {`ConstraintPtr', `Double'} -> `()' #}

-- | Get the percentage of joint error that remains unfixed after a second.
{# fun unsafe cpConstraintGetErrorBias as constraintGetErrorBias {`ConstraintPtr'} -> `Double' #}

-- | Set the percentage of joint error that remains unfixed after a second.
{# fun unsafe cpConstraintSetErrorBias as constraintSetErrorBias {`ConstraintPtr', `Double'} -> `()' #}

-- | Get the maximum speed at which the constraint can apply error correction. Defaults to INFINITY.
{# fun unsafe cpConstraintGetMaxBias as constraintGetMaxBias {`ConstraintPtr'} -> `Double' #}

-- | Set the maximum speed at which the constraint can apply error correction.
{# fun unsafe cpConstraintSetMaxBias as constraintSetMaxBias {`ConstraintPtr', `Double'} -> `()' #}

-- | Get the 'SpacePtr' that constraint has been added to.
{# fun unsafe cpConstraintGetSpace as constraintGetSpace {`ConstraintPtr'} -> `SpacePtr' #}

-- | Do bodies collide?
{# fun unsafe cpConstraintGetCollideBodies as constraintGetCollideBodies {`ConstraintPtr'} -> `Bool' #}

-- | Set if bodies collide. This can be used to create a chain that self collides,
-- but adjacent links in the chain do not collide.
{# fun unsafe cpConstraintSetCollideBodies as constraintSetCollideBodies {`ConstraintPtr', `Bool'} -> `()' #}

-- | Get the user definable data pointer.
{# fun unsafe cpConstraintGetUserData as constraintGetUserData {`ConstraintPtr'} -> `DataPtr' #}

-- | Set a user definable data pointer. Use this pointer to get a reference to the game object
-- that owns this constraint from callbacks.
{# fun unsafe cpConstraintSetUserData as constraintSetUserData {`ConstraintPtr', `DataPtr'} -> `()' #}

-- | The most recent impulse that constraint applied. To convert this to a force,
-- divide by the timestep passed to 'spaceStep'. You can use this to implement breakable joints
-- to check if the force they attempted to apply exceeded a certain threshold.
{# fun unsafe cpConstraintGetImpulse as constraintGetImpulse {`ConstraintPtr'} -> `Double' #}

-- | Free function is shared by all joint types. Allocation functions are specific to each joint type.
{# fun unsafe cpConstraintFree as constraintFree {`ConstraintPtr'} -> `()' #}

-- | Connect two bodies via anchor points on those bodies. The distance between the two anchor points is measured
-- when the joint is created. If you want to set a specific distance, use the setter function to override it.
{# fun unsafe cpPinJointNew as pinJointNew
  { `BodyPtr'     -- ^ First body to connect
  , `BodyPtr'     -- ^ Second body to connect
  , with* %`Vect' -- ^ First anchor
  , with* %`Vect' -- ^ Second anchor
  } -> `ConstraintPtr' #}

-- | Get anchor on first body.
{# fun unsafe w_cpPinJointGetAnchorA as pinJointGetAnchorA {`ConstraintPtr', alloca- `Vect' peek*} -> `()' #}

-- | Set anchor on first body.
{# fun unsafe cpPinJointSetAnchorA as pinJointSetAnchorA {`ConstraintPtr', with* %`Vect'} -> `()' #}

-- | Get anchor on second body.
{# fun unsafe w_cpPinJointGetAnchorB as pinJointGetAnchorB {`ConstraintPtr', alloca- `Vect' peek*} -> `()' #}

-- | Set anchor on second body.
{# fun unsafe cpPinJointSetAnchorB as pinJointSetAnchorB {`ConstraintPtr', with* %`Vect'} -> `()' #}

-- | Get desired distance the joint will try to enforce.
{# fun unsafe cpPinJointGetDist as pinJointGetDist {`ConstraintPtr'} -> `Double' #}

-- | Set desired distance the joint will try to enforce.
{# fun unsafe cpPinJointSetDist as pinJointSetDist {`ConstraintPtr', `Double'} -> `()' #}

-- | Connect two bodies via anchor points forcing distance to remain in range.
{# fun unsafe cpSlideJointNew as slideJointNew
  { `BodyPtr'     -- ^ First body to connect
  , `BodyPtr'     -- ^ Second body to connect
  , with* %`Vect' -- ^ First anchor
  , with* %`Vect' -- ^ Second anchor
  , `Double'      -- ^ Minimum allowed distance
  , `Double'      -- ^ Maximum allowed distance
  } -> `ConstraintPtr' #}

-- | Get anchor on first body.
{# fun unsafe w_cpSlideJointGetAnchorA as slideJointGetAnchorA {`ConstraintPtr', alloca- `Vect' peek*} -> `()' #}

-- | Set anchor on first body.
{# fun unsafe cpSlideJointSetAnchorA as slideJointSetAnchorA {`ConstraintPtr', with* %`Vect'} -> `()' #}

-- | Get anchor on second body.
{# fun unsafe w_cpSlideJointGetAnchorB as slideJointGetAnchorB {`ConstraintPtr', alloca- `Vect' peek*} -> `()' #}

-- | Set anchor on second body.
{# fun unsafe cpSlideJointSetAnchorB as slideJointSetAnchorB {`ConstraintPtr', with* %`Vect'} -> `()' #}

-- | Get minimum distance the joint will try to enforce.
{# fun unsafe cpSlideJointGetMin as slideJointGetMin {`ConstraintPtr'} -> `Double' #}

-- | Set minimum distance the joint will try to enforce.
{# fun unsafe cpSlideJointSetMin as slideJointSetMin {`ConstraintPtr', `Double'} -> `()' #}

-- | Get maximum distance the joint will try to enforce.
{# fun unsafe cpSlideJointGetMax as slideJointGetMax {`ConstraintPtr'} -> `Double' #}

-- | Set maximum distance the joint will try to enforce.
{# fun unsafe cpSlideJointSetMax as slideJointSetMax {`ConstraintPtr', `Double'} -> `()' #}

-- | Because the pivot location is given in world coordinates,
-- you must have the bodies moved into the correct positions already.
{# fun unsafe cpPivotJointNew as pivotJointNew
  { `BodyPtr'     -- ^ First body to connect
  , `BodyPtr'     -- ^ Second body to connect
  , with* %`Vect' -- ^ Point in the world coordinates of the pivot
  } -> `ConstraintPtr' #}

-- | Alternatively you can specify the joint based on a pair of anchor points,
-- but make sure you have the bodies in the right place as the joint will fix itself
-- as soon as you start simulating the space.
{# fun unsafe cpPivotJointNew2 as pivotJointNew2
  { `BodyPtr'     -- ^ First body to connect
  , `BodyPtr'     -- ^ Second body to connect
  , with* %`Vect' -- ^ Anchor on first body
  , with* %`Vect' -- ^ Anchor on second body
  } -> `ConstraintPtr' #}

-- | Get anchor on first body.
{# fun unsafe w_cpPivotJointGetAnchorA as pivotJointGetAnchorA {`ConstraintPtr', alloca- `Vect' peek*} -> `()' #}

-- | Set anchor on first body.
{# fun unsafe cpPivotJointSetAnchorA as pivotJointSetAnchorA {`ConstraintPtr', with* %`Vect'} -> `()' #}

-- | Get anchor on second body.
{# fun unsafe w_cpPivotJointGetAnchorB as pivotJointGetAnchorB {`ConstraintPtr', alloca- `Vect' peek*} -> `()' #}

-- | Set anchor on second body.
{# fun unsafe cpPivotJointSetAnchorB as pivotJointSetAnchorB {`ConstraintPtr', with* %`Vect'} -> `()' #}

-- | Pivot is attached to groove on first body and to anchor on the second. All coordinates are body local.
{# fun unsafe cpGrooveJointNew as grooveJointNew
  { `BodyPtr'     -- ^ First body to connect
  , `BodyPtr'     -- ^ Second body to connect
  , with* %`Vect' -- ^ First endpoint of groove (on first body)
  , with* %`Vect' -- ^ Second endpoint of groove (on first body)
  , with* %`Vect' -- ^ Anchor (on second body)
  } -> `ConstraintPtr' #}

-- | Get first endpoint of groove.
{# fun unsafe w_cpGrooveJointGetGrooveA as grooveJointGetGrooveA {`ConstraintPtr', alloca- `Vect' peek*} -> `()' #}

-- | Set first endpoint of groove.
{# fun unsafe cpGrooveJointSetGrooveA as grooveJointSetGrooveA {`ConstraintPtr', with* %`Vect'} -> `()' #}

-- | Get second endpoint of groove.
{# fun unsafe w_cpGrooveJointGetGrooveB as grooveJointGetGrooveB {`ConstraintPtr', alloca- `Vect' peek*} -> `()' #}

-- | Set second endpoint of groove.
{# fun unsafe cpGrooveJointSetGrooveB as grooveJointSetGrooveB {`ConstraintPtr', with* %`Vect'} -> `()' #}

-- | Get anchor on second body.
{# fun unsafe w_cpGrooveJointGetAnchorB as grooveJointGetAnchorB {`ConstraintPtr', alloca- `Vect' peek*} -> `()' #}

-- | Set anchor on second body.
{# fun unsafe cpGrooveJointSetAnchorB as grooveJointSetAnchorB {`ConstraintPtr', with* %`Vect'} -> `()' #}

-- | Defined much like a slide joint.
{# fun unsafe cpDampedSpringNew as dampedSpringNew
  { `BodyPtr'     -- ^ First body to connect
  , `BodyPtr'     -- ^ Second body to connect
  , with* %`Vect' -- ^ First anchor
  , with* %`Vect' -- ^ Second anchor
  , `Double'      -- ^ Distance the spring wants to be
  , `Double'      -- ^ Spring constant (<http://en.wikipedia.org/wiki/Young's_modulus Young's modulus>)
  , `Double'      -- ^ How soft to make damping of the spring
  } -> `ConstraintPtr' #}

-- | Get anchor on first body.
{# fun unsafe w_cpDampedSpringGetAnchorA as dampedSpringGetAnchorA {`ConstraintPtr', alloca- `Vect' peek*} -> `()' #}

-- | Set anchor on first body.
{# fun unsafe cpDampedSpringSetAnchorA as dampedSpringSetAnchorA {`ConstraintPtr', with* %`Vect'} -> `()' #}

-- | Get anchor on second body.
{# fun unsafe w_cpDampedSpringGetAnchorB as dampedSpringGetAnchorB {`ConstraintPtr', alloca- `Vect' peek*} -> `()' #}

-- | Set anchor on second body.
{# fun unsafe cpDampedSpringSetAnchorB as dampedSpringSetAnchorB {`ConstraintPtr', with* %`Vect'} -> `()' #}

-- | Get desired distance the spring will try to enforce.
{# fun unsafe cpDampedSpringGetRestLength as dampedSpringGetRestLength {`ConstraintPtr'} -> `Double' #}

-- | Set desired distance the spring will try to enforce.
{# fun unsafe cpDampedSpringSetRestLength as dampedSpringSetRestLength {`ConstraintPtr', `Double'} -> `()' #}

-- | Get spring stiffness.
{# fun unsafe cpDampedSpringGetStiffness as dampedSpringGetStiffness {`ConstraintPtr'} -> `Double' #}

-- | Set desired stiffness.
{# fun unsafe cpDampedSpringSetStiffness as dampedSpringSetStiffness {`ConstraintPtr', `Double'} -> `()' #}

-- | Get spring damping.
{# fun unsafe cpDampedSpringGetDamping as dampedSpringGetDamping {`ConstraintPtr'} -> `Double' #}

-- | Set spring damping.
{# fun unsafe cpDampedSpringSetDamping as dampedSpringSetDamping {`ConstraintPtr', `Double'} -> `()' #}

-- | Create new damped rotary spring constraint
{# fun unsafe cpDampedRotarySpringNew as dampedRotarySpringNew
  { `BodyPtr' -- ^ First body to connect
  , `BodyPtr' -- ^ Second body to connect
  , `Double'  -- ^ Relative angle in radians that the bodies want to have
  , `Double'  -- ^ Spring constant (stiffness)
  , `Double'  -- ^ Spring damping
  } -> `ConstraintPtr' #}

-- | Get desired angle in radians the spring will try to enforce.
{# fun unsafe cpDampedRotarySpringGetRestAngle as dampedRotarySpringGetRestAngle {`ConstraintPtr'} -> `Double' #}

-- | Set desired angle in radians the spring will try to enforce.
{# fun unsafe cpDampedRotarySpringSetRestAngle as dampedRotarySpringSetRestAngle {`ConstraintPtr', `Double'} -> `()' #}

-- | Get spring stiffness.
{# fun unsafe cpDampedRotarySpringGetStiffness as dampedRotarySpringGetStiffness {`ConstraintPtr'} -> `Double' #}

-- | Set desired stiffness.
{# fun unsafe cpDampedRotarySpringSetStiffness as dampedRotarySpringSetStiffness {`ConstraintPtr', `Double'} -> `()' #}

-- | Get spring damping.
{# fun unsafe cpDampedRotarySpringGetDamping as dampedRotarySpringGetDamping {`ConstraintPtr'} -> `Double' #}

-- | Set spring damping.
{# fun unsafe cpDampedRotarySpringSetDamping as dampedRotarySpringSetDamping {`ConstraintPtr', `Double'} -> `()' #}

-- | Create new rotation limiting joint
{# fun unsafe cpRotaryLimitJointNew as rotaryLimitJointNew
  { `BodyPtr' -- ^ First body to connect
  , `BodyPtr' -- ^ Second body to connect
  , `Double'  -- ^ Minimum angle in radians the joint will enforce
  , `Double'  -- ^ Maximum angle in radians the joint will enforce
  } -> `ConstraintPtr' #}

-- | Get minimum angle in radians the joint will try to enforce.
{# fun unsafe cpRotaryLimitJointGetMin as rotaryLimitJointGetMin {`ConstraintPtr'} -> `Double' #}

-- | Set minimum angle in radians the joint will try to enforce.
{# fun unsafe cpRotaryLimitJointSetMin as rotaryLimitJointSetMin {`ConstraintPtr', `Double'} -> `()' #}

-- | Get maximum angle in radians the joint will try to enforce.
{# fun unsafe cpRotaryLimitJointGetMax as rotaryLimitJointGetMax {`ConstraintPtr'} -> `Double' #}

-- | Set maximum angle in radians the joint will try to enforce.
{# fun unsafe cpRotaryLimitJointSetMax as rotaryLimitJointSetMax {`ConstraintPtr', `Double'} -> `()' #}

-- | Allocate and initialize a ratchet joint.
{# fun unsafe cpRatchetJointNew as ratchetJointNew
  { `BodyPtr' -- ^ First body to connect
  , `BodyPtr' -- ^ Second body to connect
  , `Double'  -- ^ The initial offset to use when deciding where the ratchet angles are.
  , `Double'  -- ^ The distance between “clicks”
  } -> `ConstraintPtr' #}

-- | Get the angle of the current ratchet tooth.
{# fun unsafe cpRatchetJointGetAngle as ratchetJointGetAngle {`ConstraintPtr'} -> `Double' #}

-- | Set the angle of the current ratchet tooth.
{# fun unsafe cpRatchetJointSetAngle as ratchetJointSetAngle {`ConstraintPtr', `Double'} -> `()' #}

-- | Get the phase offset of the ratchet.
{# fun unsafe cpRatchetJointGetPhase as ratchetJointGetPhase {`ConstraintPtr'} -> `Double' #}

-- | Set the phase offset of the ratchet.
{# fun unsafe cpRatchetJointSetPhase as ratchetJointSetPhase {`ConstraintPtr', `Double'} -> `()' #}

-- | Get the angular distance of each ratchet.
{# fun unsafe cpRatchetJointGetRatchet as ratchetJointGetRatchet {`ConstraintPtr'} -> `Double' #}

-- | Set the angular distance of each ratchet.
{# fun unsafe cpRatchetJointSetRatchet as ratchetJointSetRatchet {`ConstraintPtr', `Double'} -> `()' #}

-- | Allocate and initialize a gear joint.
{# fun unsafe cpGearJointNew as gearJointNew
  { `BodyPtr' -- ^ First body to connect
  , `BodyPtr' -- ^ Second body to connect
  , `Double'  -- ^ The initial angular offset of the two bodies.
  , `Double'  -- ^ Ratio measures in absolute terms
  } -> `ConstraintPtr' #}

-- | Get the phase offset of the ratchet.
{# fun unsafe cpGearJointGetPhase as gearJointGetPhase {`ConstraintPtr'} -> `Double' #}

-- | Set the phase offset of the ratchet.
{# fun unsafe cpGearJointSetPhase as gearJointSetPhase {`ConstraintPtr', `Double'} -> `()' #}

-- | Get the ratio
{# fun unsafe cpGearJointGetRatio as gearJointGetRatio {`ConstraintPtr'} -> `Double' #}

-- | Set the ratio
{# fun unsafe cpGearJointSetRatio as gearJointSetRatio {`ConstraintPtr', `Double'} -> `()' #}

-- | Allocate and initialize a simple motor.
{# fun unsafe cpSimpleMotorNew as simpleMotorNew
  { `BodyPtr' -- ^ First body to connect
  , `BodyPtr' -- ^ Second body to connect
  , `Double'  -- ^ The desired relative angular velocity.
  } -> `ConstraintPtr' #}

-- | Get relative angular velocity.
{# fun unsafe cpSimpleMotorGetRate as simpleMotorGetRate {`ConstraintPtr'} -> `Double' #}

-- | Set the desired relative angular velocity.
{# fun unsafe cpSimpleMotorSetRate as simpleMotorSetRate {`ConstraintPtr', `Double'} -> `()' #}
