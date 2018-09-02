-- | Description: Dealing with joints/constraints
-- Module defines utilities for operations with constraints.
module Chiphunk.Low.Constraint
  ( Constraint
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
{# fun unsafe cpConstraintGetBodyA as constraintGetBodyA {`Constraint'} -> `Body' #}

-- | Get the second body constraint is attached to
{# fun unsafe cpConstraintGetBodyB as constraintGetBodyB {`Constraint'} -> `Body' #}

-- | Get the maximum force that the constraint can use to act on the two bodies. Defaults to INFINITY.
{# fun unsafe cpConstraintGetMaxForce as constraintGetMaxForce {`Constraint'} -> `Double' #}

-- | Set the maximum force that the constraint can use to act on the two bodies.
{# fun unsafe cpConstraintSetMaxForce as constraintSetMaxForce {`Constraint', `Double'} -> `()' #}

-- | Get the percentage of joint error that remains unfixed after a second.
{# fun unsafe cpConstraintGetErrorBias as constraintGetErrorBias {`Constraint'} -> `Double' #}

-- | Set the percentage of joint error that remains unfixed after a second.
{# fun unsafe cpConstraintSetErrorBias as constraintSetErrorBias {`Constraint', `Double'} -> `()' #}

-- | Get the maximum speed at which the constraint can apply error correction. Defaults to INFINITY.
{# fun unsafe cpConstraintGetMaxBias as constraintGetMaxBias {`Constraint'} -> `Double' #}

-- | Set the maximum speed at which the constraint can apply error correction.
{# fun unsafe cpConstraintSetMaxBias as constraintSetMaxBias {`Constraint', `Double'} -> `()' #}

-- | Get the 'Space' that constraint has been added to.
{# fun unsafe cpConstraintGetSpace as constraintGetSpace {`Constraint'} -> `Space' #}

-- | Do bodies collide?
{# fun unsafe cpConstraintGetCollideBodies as constraintGetCollideBodies {`Constraint'} -> `Bool' #}

-- | Set if bodies collide. This can be used to create a chain that self collides,
-- but adjacent links in the chain do not collide.
{# fun unsafe cpConstraintSetCollideBodies as constraintSetCollideBodies {`Constraint', `Bool'} -> `()' #}

-- | Get the user definable data pointer.
{# fun unsafe cpConstraintGetUserData as constraintGetUserData {`Constraint'} -> `DataPtr' #}

-- | Set a user definable data pointer. Use this pointer to get a reference to the game object
-- that owns this constraint from callbacks.
{# fun unsafe cpConstraintSetUserData as constraintSetUserData {`Constraint', `DataPtr'} -> `()' #}

-- | The most recent impulse that constraint applied. To convert this to a force,
-- divide by the timestep passed to 'spaceStep'. You can use this to implement breakable joints
-- to check if the force they attempted to apply exceeded a certain threshold.
{# fun unsafe cpConstraintGetImpulse as constraintGetImpulse {`Constraint'} -> `Double' #}

-- | Free function is shared by all joint types. Allocation functions are specific to each joint type.
{# fun unsafe cpConstraintFree as constraintFree {`Constraint'} -> `()' #}

-- | Connect two bodies via anchor points on those bodies. The distance between the two anchor points is measured
-- when the joint is created. If you want to set a specific distance, use the setter function to override it.
{# fun unsafe cpPinJointNew as pinJointNew
  { `Body'        -- ^ First body to connect
  , `Body'        -- ^ Second body to connect
  , with* %`Vect' -- ^ First anchor
  , with* %`Vect' -- ^ Second anchor
  } -> `Constraint' #}

-- | Get anchor on first body.
{# fun unsafe w_cpPinJointGetAnchorA as pinJointGetAnchorA {`Constraint', alloca- `Vect' peek*} -> `()' #}

-- | Set anchor on first body.
{# fun unsafe cpPinJointSetAnchorA as pinJointSetAnchorA {`Constraint', with* %`Vect'} -> `()' #}

-- | Get anchor on second body.
{# fun unsafe w_cpPinJointGetAnchorB as pinJointGetAnchorB {`Constraint', alloca- `Vect' peek*} -> `()' #}

-- | Set anchor on second body.
{# fun unsafe cpPinJointSetAnchorB as pinJointSetAnchorB {`Constraint', with* %`Vect'} -> `()' #}

-- | Get desired distance the joint will try to enforce.
{# fun unsafe cpPinJointGetDist as pinJointGetDist {`Constraint'} -> `Double' #}

-- | Set desired distance the joint will try to enforce.
{# fun unsafe cpPinJointSetDist as pinJointSetDist {`Constraint', `Double'} -> `()' #}

-- | Connect two bodies via anchor points forcing distance to remain in range.
{# fun unsafe cpSlideJointNew as slideJointNew
  { `Body'        -- ^ First body to connect
  , `Body'        -- ^ Second body to connect
  , with* %`Vect' -- ^ First anchor
  , with* %`Vect' -- ^ Second anchor
  , `Double'      -- ^ Minimum allowed distance
  , `Double'      -- ^ Maximum allowed distance
  } -> `Constraint' #}

-- | Get anchor on first body.
{# fun unsafe w_cpSlideJointGetAnchorA as slideJointGetAnchorA {`Constraint', alloca- `Vect' peek*} -> `()' #}

-- | Set anchor on first body.
{# fun unsafe cpSlideJointSetAnchorA as slideJointSetAnchorA {`Constraint', with* %`Vect'} -> `()' #}

-- | Get anchor on second body.
{# fun unsafe w_cpSlideJointGetAnchorB as slideJointGetAnchorB {`Constraint', alloca- `Vect' peek*} -> `()' #}

-- | Set anchor on second body.
{# fun unsafe cpSlideJointSetAnchorB as slideJointSetAnchorB {`Constraint', with* %`Vect'} -> `()' #}

-- | Get minimum distance the joint will try to enforce.
{# fun unsafe cpSlideJointGetMin as slideJointGetMin {`Constraint'} -> `Double' #}

-- | Set minimum distance the joint will try to enforce.
{# fun unsafe cpSlideJointSetMin as slideJointSetMin {`Constraint', `Double'} -> `()' #}

-- | Get maximum distance the joint will try to enforce.
{# fun unsafe cpSlideJointGetMax as slideJointGetMax {`Constraint'} -> `Double' #}

-- | Set maximum distance the joint will try to enforce.
{# fun unsafe cpSlideJointSetMax as slideJointSetMax {`Constraint', `Double'} -> `()' #}

-- | Because the pivot location is given in world coordinates,
-- you must have the bodies moved into the correct positions already.
{# fun unsafe cpPivotJointNew as pivotJointNew
  { `Body'        -- ^ First body to connect
  , `Body'        -- ^ Second body to connect
  , with* %`Vect' -- ^ Point in the world coordinates of the pivot
  } -> `Constraint' #}

-- | Alternatively you can specify the joint based on a pair of anchor points,
-- but make sure you have the bodies in the right place as the joint will fix itself
-- as soon as you start simulating the space.
{# fun unsafe cpPivotJointNew2 as pivotJointNew2
  { `Body'        -- ^ First body to connect
  , `Body'        -- ^ Second body to connect
  , with* %`Vect' -- ^ Anchor on first body
  , with* %`Vect' -- ^ Anchor on second body
  } -> `Constraint' #}

-- | Get anchor on first body.
{# fun unsafe w_cpPivotJointGetAnchorA as pivotJointGetAnchorA {`Constraint', alloca- `Vect' peek*} -> `()' #}

-- | Set anchor on first body.
{# fun unsafe cpPivotJointSetAnchorA as pivotJointSetAnchorA {`Constraint', with* %`Vect'} -> `()' #}

-- | Get anchor on second body.
{# fun unsafe w_cpPivotJointGetAnchorB as pivotJointGetAnchorB {`Constraint', alloca- `Vect' peek*} -> `()' #}

-- | Set anchor on second body.
{# fun unsafe cpPivotJointSetAnchorB as pivotJointSetAnchorB {`Constraint', with* %`Vect'} -> `()' #}

-- | Pivot is attached to groove on first body and to anchor on the second. All coordinates are body local.
{# fun unsafe cpGrooveJointNew as grooveJointNew
  { `Body'        -- ^ First body to connect
  , `Body'        -- ^ Second body to connect
  , with* %`Vect' -- ^ First endpoint of groove (on first body)
  , with* %`Vect' -- ^ Second endpoint of groove (on first body)
  , with* %`Vect' -- ^ Anchor (on second body)
  } -> `Constraint' #}

-- | Get first endpoint of groove.
{# fun unsafe w_cpGrooveJointGetGrooveA as grooveJointGetGrooveA {`Constraint', alloca- `Vect' peek*} -> `()' #}

-- | Set first endpoint of groove.
{# fun unsafe cpGrooveJointSetGrooveA as grooveJointSetGrooveA {`Constraint', with* %`Vect'} -> `()' #}

-- | Get second endpoint of groove.
{# fun unsafe w_cpGrooveJointGetGrooveB as grooveJointGetGrooveB {`Constraint', alloca- `Vect' peek*} -> `()' #}

-- | Set second endpoint of groove.
{# fun unsafe cpGrooveJointSetGrooveB as grooveJointSetGrooveB {`Constraint', with* %`Vect'} -> `()' #}

-- | Get anchor on second body.
{# fun unsafe w_cpGrooveJointGetAnchorB as grooveJointGetAnchorB {`Constraint', alloca- `Vect' peek*} -> `()' #}

-- | Set anchor on second body.
{# fun unsafe cpGrooveJointSetAnchorB as grooveJointSetAnchorB {`Constraint', with* %`Vect'} -> `()' #}

-- | Defined much like a slide joint.
{# fun unsafe cpDampedSpringNew as dampedSpringNew
  { `Body'        -- ^ First body to connect
  , `Body'        -- ^ Second body to connect
  , with* %`Vect' -- ^ First anchor
  , with* %`Vect' -- ^ Second anchor
  , `Double'      -- ^ Distance the spring wants to be
  , `Double'      -- ^ Spring constant (<http://en.wikipedia.org/wiki/Young's_modulus Young's modulus>)
  , `Double'      -- ^ How soft to make damping of the spring
  } -> `Constraint' #}

-- | Get anchor on first body.
{# fun unsafe w_cpDampedSpringGetAnchorA as dampedSpringGetAnchorA {`Constraint', alloca- `Vect' peek*} -> `()' #}

-- | Set anchor on first body.
{# fun unsafe cpDampedSpringSetAnchorA as dampedSpringSetAnchorA {`Constraint', with* %`Vect'} -> `()' #}

-- | Get anchor on second body.
{# fun unsafe w_cpDampedSpringGetAnchorB as dampedSpringGetAnchorB {`Constraint', alloca- `Vect' peek*} -> `()' #}

-- | Set anchor on second body.
{# fun unsafe cpDampedSpringSetAnchorB as dampedSpringSetAnchorB {`Constraint', with* %`Vect'} -> `()' #}

-- | Get desired distance the spring will try to enforce.
{# fun unsafe cpDampedSpringGetRestLength as dampedSpringGetRestLength {`Constraint'} -> `Double' #}

-- | Set desired distance the spring will try to enforce.
{# fun unsafe cpDampedSpringSetRestLength as dampedSpringSetRestLength {`Constraint', `Double'} -> `()' #}

-- | Get spring stiffness.
{# fun unsafe cpDampedSpringGetStiffness as dampedSpringGetStiffness {`Constraint'} -> `Double' #}

-- | Set desired stiffness.
{# fun unsafe cpDampedSpringSetStiffness as dampedSpringSetStiffness {`Constraint', `Double'} -> `()' #}

-- | Get spring damping.
{# fun unsafe cpDampedSpringGetDamping as dampedSpringGetDamping {`Constraint'} -> `Double' #}

-- | Set spring damping.
{# fun unsafe cpDampedSpringSetDamping as dampedSpringSetDamping {`Constraint', `Double'} -> `()' #}

-- | Create new damped rotary spring constraint
{# fun unsafe cpDampedRotarySpringNew as dampedRotarySpringNew
  { `Body'    -- ^ First body to connect
  , `Body'    -- ^ Second body to connect
  , `Double'  -- ^ Relative angle in radians that the bodies want to have
  , `Double'  -- ^ Spring constant (stiffness)
  , `Double'  -- ^ Spring damping
  } -> `Constraint' #}

-- | Get desired angle in radians the spring will try to enforce.
{# fun unsafe cpDampedRotarySpringGetRestAngle as dampedRotarySpringGetRestAngle {`Constraint'} -> `Double' #}

-- | Set desired angle in radians the spring will try to enforce.
{# fun unsafe cpDampedRotarySpringSetRestAngle as dampedRotarySpringSetRestAngle {`Constraint', `Double'} -> `()' #}

-- | Get spring stiffness.
{# fun unsafe cpDampedRotarySpringGetStiffness as dampedRotarySpringGetStiffness {`Constraint'} -> `Double' #}

-- | Set desired stiffness.
{# fun unsafe cpDampedRotarySpringSetStiffness as dampedRotarySpringSetStiffness {`Constraint', `Double'} -> `()' #}

-- | Get spring damping.
{# fun unsafe cpDampedRotarySpringGetDamping as dampedRotarySpringGetDamping {`Constraint'} -> `Double' #}

-- | Set spring damping.
{# fun unsafe cpDampedRotarySpringSetDamping as dampedRotarySpringSetDamping {`Constraint', `Double'} -> `()' #}

-- | Create new rotation limiting joint
{# fun unsafe cpRotaryLimitJointNew as rotaryLimitJointNew
  { `Body'    -- ^ First body to connect
  , `Body'    -- ^ Second body to connect
  , `Double'  -- ^ Minimum angle in radians the joint will enforce
  , `Double'  -- ^ Maximum angle in radians the joint will enforce
  } -> `Constraint' #}

-- | Get minimum angle in radians the joint will try to enforce.
{# fun unsafe cpRotaryLimitJointGetMin as rotaryLimitJointGetMin {`Constraint'} -> `Double' #}

-- | Set minimum angle in radians the joint will try to enforce.
{# fun unsafe cpRotaryLimitJointSetMin as rotaryLimitJointSetMin {`Constraint', `Double'} -> `()' #}

-- | Get maximum angle in radians the joint will try to enforce.
{# fun unsafe cpRotaryLimitJointGetMax as rotaryLimitJointGetMax {`Constraint'} -> `Double' #}

-- | Set maximum angle in radians the joint will try to enforce.
{# fun unsafe cpRotaryLimitJointSetMax as rotaryLimitJointSetMax {`Constraint', `Double'} -> `()' #}

-- | Allocate and initialize a ratchet joint.
{# fun unsafe cpRatchetJointNew as ratchetJointNew
  { `Body'    -- ^ First body to connect
  , `Body'    -- ^ Second body to connect
  , `Double'  -- ^ The initial offset to use when deciding where the ratchet angles are.
  , `Double'  -- ^ The distance between “clicks”
  } -> `Constraint' #}

-- | Get the angle of the current ratchet tooth.
{# fun unsafe cpRatchetJointGetAngle as ratchetJointGetAngle {`Constraint'} -> `Double' #}

-- | Set the angle of the current ratchet tooth.
{# fun unsafe cpRatchetJointSetAngle as ratchetJointSetAngle {`Constraint', `Double'} -> `()' #}

-- | Get the phase offset of the ratchet.
{# fun unsafe cpRatchetJointGetPhase as ratchetJointGetPhase {`Constraint'} -> `Double' #}

-- | Set the phase offset of the ratchet.
{# fun unsafe cpRatchetJointSetPhase as ratchetJointSetPhase {`Constraint', `Double'} -> `()' #}

-- | Get the angular distance of each ratchet.
{# fun unsafe cpRatchetJointGetRatchet as ratchetJointGetRatchet {`Constraint'} -> `Double' #}

-- | Set the angular distance of each ratchet.
{# fun unsafe cpRatchetJointSetRatchet as ratchetJointSetRatchet {`Constraint', `Double'} -> `()' #}

-- | Allocate and initialize a gear joint.
{# fun unsafe cpGearJointNew as gearJointNew
  { `Body'    -- ^ First body to connect
  , `Body'    -- ^ Second body to connect
  , `Double'  -- ^ The initial angular offset of the two bodies.
  , `Double'  -- ^ Ratio measures in absolute terms
  } -> `Constraint' #}

-- | Get the phase offset of the ratchet.
{# fun unsafe cpGearJointGetPhase as gearJointGetPhase {`Constraint'} -> `Double' #}

-- | Set the phase offset of the ratchet.
{# fun unsafe cpGearJointSetPhase as gearJointSetPhase {`Constraint', `Double'} -> `()' #}

-- | Get the ratio
{# fun unsafe cpGearJointGetRatio as gearJointGetRatio {`Constraint'} -> `Double' #}

-- | Set the ratio
{# fun unsafe cpGearJointSetRatio as gearJointSetRatio {`Constraint', `Double'} -> `()' #}

-- | Allocate and initialize a simple motor.
{# fun unsafe cpSimpleMotorNew as simpleMotorNew
  { `Body'    -- ^ First body to connect
  , `Body'    -- ^ Second body to connect
  , `Double'  -- ^ The desired relative angular velocity.
  } -> `Constraint' #}

-- | Get relative angular velocity.
{# fun unsafe cpSimpleMotorGetRate as simpleMotorGetRate {`Constraint'} -> `Double' #}

-- | Set the desired relative angular velocity.
{# fun unsafe cpSimpleMotorSetRate as simpleMotorSetRate {`Constraint', `Double'} -> `()' #}
