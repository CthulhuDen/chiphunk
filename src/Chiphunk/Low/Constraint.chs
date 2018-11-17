-- | Description: Dealing with joints/constraints
-- Module defines utilities for operations with constraints.
module Chiphunk.Low.Constraint
  ( Constraint
  , constraintBodyA
  , constraintBodyB
  , constraintMaxForce
  , constraintErrorBias
  , constraintMaxBias
  , constraintSpace
  , constraintCollideBodies
  , constraintUserData
  , constraintImpulse
  , constraintFree
  , pinJointNew
  , pinJointAnchorA
  , pinJointAnchorB
  , pinJointDist
  , slideJointNew
  , slideJointAnchorA
  , slideJointAnchorB
  , slideJointMin
  , slideJointMax
  , pivotJointNew
  , pivotJointNew2
  , pivotJointAnchorA
  , pivotJointAnchorB
  , grooveJointNew
  , grooveJointGrooveA
  , grooveJointGrooveB
  , grooveJointAnchorB
  , dampedSpringNew
  , dampedSpringAnchorA
  , dampedSpringAnchorB
  , dampedSpringRestLength
  , dampedSpringStiffness
  , dampedSpringDamping
  , dampedRotarySpringNew
  , dampedRotarySpringRestAngle
  , dampedRotarySpringStiffness
  , dampedRotarySpringDamping
  , rotaryLimitJointNew
  , rotaryLimitJointMin
  , rotaryLimitJointMax
  , ratchetJointNew
  , ratchetJointAngle
  , ratchetJointPhase
  , ratchetJointRatchet
  , gearJointNew
  , gearJointPhase
  , gearJointRatio

  , simpleMotorNew
  , simpleMotorRate
  ) where

import Data.StateVar
import Foreign

{# import Chiphunk.Low.Types #}

#include <chipmunk/chipmunk.h>
#include <wrapper.h>

{# fun unsafe cpConstraintGetBodyA {`Constraint'} -> `Body' #}

-- | The first body constraint is attached to
constraintBodyA :: Constraint -> GettableStateVar Body
constraintBodyA = makeGettableStateVar . cpConstraintGetBodyA

{# fun unsafe cpConstraintGetBodyB {`Constraint'} -> `Body' #}

-- | The second body constraint is attached to
constraintBodyB :: Constraint -> GettableStateVar Body
constraintBodyB = makeGettableStateVar . cpConstraintGetBodyB

{# fun unsafe cpConstraintGetMaxForce {`Constraint'} -> `Double' #}

{# fun unsafe cpConstraintSetMaxForce {`Constraint', `Double'} -> `()' #}

-- | The maximum force that the constraint can use to act on the two bodies.
-- Defaults to INFINITY.
constraintMaxForce :: Constraint -> StateVar Double
constraintMaxForce = mkStateVar cpConstraintGetMaxForce cpConstraintSetMaxForce

{# fun unsafe cpConstraintGetErrorBias {`Constraint'} -> `Double' #}

{# fun unsafe cpConstraintSetErrorBias {`Constraint', `Double'} -> `()' #}

-- | The percentage of joint error that remains unfixed after a second.
-- This works exactly the same as the collision bias property of a space,
-- but applies to fixing error (stretching) of joints instead of overlapping collisions.
constraintErrorBias :: Constraint -> StateVar Double
constraintErrorBias = mkStateVar cpConstraintGetErrorBias cpConstraintSetErrorBias

{# fun unsafe cpConstraintGetMaxBias {`Constraint'} -> `Double' #}

{# fun unsafe cpConstraintSetMaxBias {`Constraint', `Double'} -> `()' #}

-- | Get the maximum speed at which the constraint can apply error correction.
-- Defaults to INFINITY.
constraintMaxBias :: Constraint -> StateVar Double
constraintMaxBias = mkStateVar cpConstraintGetMaxBias cpConstraintSetMaxBias

{# fun unsafe cpConstraintGetSpace {`Constraint'} -> `Space' #}

-- | The 'Space' that constraint has been added to.
constraintSpace :: Constraint -> GettableStateVar Space
constraintSpace = makeGettableStateVar . cpConstraintGetSpace

{# fun unsafe cpConstraintGetCollideBodies {`Constraint'} -> `Bool' #}

{# fun unsafe cpConstraintSetCollideBodies {`Constraint', `Bool'} -> `()' #}

-- | Constraints can be used for filtering collisions too.
-- When two bodies collide, Chipmunk ignores the collisions
-- if this property is set to @False@ on any constraint that connects the two bodies.
-- Defaults to @True@.
--
-- This can be used to create a chain that self collides,
-- but adjacent links in the chain do not collide.
constraintCollideBodies :: Constraint -> StateVar Bool
constraintCollideBodies = mkStateVar cpConstraintGetCollideBodies cpConstraintSetCollideBodies

{# fun unsafe cpConstraintGetUserData {`Constraint'} -> `DataPtr' #}

{# fun unsafe cpConstraintSetUserData {`Constraint', `DataPtr'} -> `()' #}

-- | A user definable data pointer.
-- Use this pointer to get a reference to the game object that owns this constraint
-- from callbacks.
constraintUserData :: Constraint -> StateVar DataPtr
constraintUserData = mkStateVar cpConstraintGetUserData cpConstraintSetUserData

{# fun unsafe cpConstraintGetImpulse {`Constraint'} -> `Double' #}

-- | The most recent impulse that constraint applied.
-- To convert this to a force, divide by the timestep passed to 'spaceStep'.
-- You can use this to implement breakable joints to check
-- if the force they attempted to apply exceeded a certain threshold.
constraintImpulse :: Constraint -> GettableStateVar Double
constraintImpulse = makeGettableStateVar . cpConstraintGetImpulse

-- | Free function is shared by all joint types. Allocation functions are specific to each joint type.
{# fun cpConstraintFree as constraintFree {`Constraint'} -> `()' #}
-- no "unsafe" qualifier because I think it may trigger separte callbacks

-- | Connect two bodies via anchor points on those bodies. The distance between the two anchor points is measured
-- when the joint is created. If you want to set a specific distance, use the setter function to override it.
{# fun unsafe cpPinJointNew as pinJointNew
  { `Body'        -- ^ First body to connect
  , `Body'        -- ^ Second body to connect
  , with* %`Vect' -- ^ First anchor
  , with* %`Vect' -- ^ Second anchor
  } -> `Constraint' #}

{# fun unsafe w_cpPinJointGetAnchorA {`Constraint', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpPinJointSetAnchorA {`Constraint', with* %`Vect'} -> `()' #}

-- | Anchor on first body.
pinJointAnchorA :: Constraint -> StateVar Vect
pinJointAnchorA = mkStateVar w_cpPinJointGetAnchorA cpPinJointSetAnchorA

{# fun unsafe w_cpPinJointGetAnchorB {`Constraint', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpPinJointSetAnchorB {`Constraint', with* %`Vect'} -> `()' #}

-- | Anchor on second body.
pinJointAnchorB :: Constraint -> StateVar Vect
pinJointAnchorB = mkStateVar w_cpPinJointGetAnchorB cpPinJointSetAnchorB

{# fun unsafe cpPinJointGetDist {`Constraint'} -> `Double' #}

{# fun unsafe cpPinJointSetDist {`Constraint', `Double'} -> `()' #}

-- | Desired distance the joint will try to enforce.
pinJointDist :: Constraint -> StateVar Double
pinJointDist = mkStateVar cpPinJointGetDist cpPinJointSetDist

-- | Connect two bodies via anchor points forcing distance to remain in range.
{# fun unsafe cpSlideJointNew as slideJointNew
  { `Body'        -- ^ First body to connect
  , `Body'        -- ^ Second body to connect
  , with* %`Vect' -- ^ First anchor
  , with* %`Vect' -- ^ Second anchor
  , `Double'      -- ^ Minimum allowed distance
  , `Double'      -- ^ Maximum allowed distance
  } -> `Constraint' #}

{# fun unsafe w_cpSlideJointGetAnchorA {`Constraint', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpSlideJointSetAnchorA  {`Constraint', with* %`Vect'} -> `()' #}

-- | Anchor on first body.
slideJointAnchorA :: Constraint -> StateVar Vect
slideJointAnchorA = mkStateVar w_cpSlideJointGetAnchorA cpSlideJointSetAnchorA

{# fun unsafe w_cpSlideJointGetAnchorB {`Constraint', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpSlideJointSetAnchorB {`Constraint', with* %`Vect'} -> `()' #}

-- | Anchor on second body.
slideJointAnchorB :: Constraint -> StateVar Vect
slideJointAnchorB = mkStateVar w_cpSlideJointGetAnchorB cpSlideJointSetAnchorB

{# fun unsafe cpSlideJointGetMin {`Constraint'} -> `Double' #}

{# fun unsafe cpSlideJointSetMin {`Constraint', `Double'} -> `()' #}

-- | The minimum distance the joint will try to enforce.
slideJointMin :: Constraint -> StateVar Double
slideJointMin = mkStateVar cpSlideJointGetMin cpSlideJointSetMin

{# fun unsafe cpSlideJointGetMax {`Constraint'} -> `Double' #}

{# fun unsafe cpSlideJointSetMax {`Constraint', `Double'} -> `()' #}

-- | The maximum distance the joint will try to enforce.
slideJointMax :: Constraint -> StateVar Double
slideJointMax = mkStateVar cpSlideJointGetMax cpSlideJointSetMax

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

{# fun unsafe w_cpPivotJointGetAnchorA {`Constraint', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpPivotJointSetAnchorA {`Constraint', with* %`Vect'} -> `()' #}

-- | Anchor on first body.
pivotJointAnchorA :: Constraint -> StateVar Vect
pivotJointAnchorA = mkStateVar w_cpPivotJointGetAnchorA cpPivotJointSetAnchorA

{# fun unsafe w_cpPivotJointGetAnchorB {`Constraint', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpPivotJointSetAnchorB {`Constraint', with* %`Vect'} -> `()' #}

-- | Anchor on second body.
pivotJointAnchorB :: Constraint -> StateVar Vect
pivotJointAnchorB = mkStateVar w_cpPivotJointGetAnchorB cpPivotJointSetAnchorB

-- | Pivot is attached to groove on first body and to anchor on the second. All coordinates are body local.
{# fun unsafe cpGrooveJointNew as grooveJointNew
  { `Body'        -- ^ First body to connect
  , `Body'        -- ^ Second body to connect
  , with* %`Vect' -- ^ First endpoint of groove (on first body)
  , with* %`Vect' -- ^ Second endpoint of groove (on first body)
  , with* %`Vect' -- ^ Anchor (on second body)
  } -> `Constraint' #}

{# fun unsafe w_cpGrooveJointGetGrooveA {`Constraint', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpGrooveJointSetGrooveA {`Constraint', with* %`Vect'} -> `()' #}

-- | First endpoint of groove (on first body).
grooveJointGrooveA :: Constraint -> StateVar Vect
grooveJointGrooveA = mkStateVar w_cpGrooveJointGetGrooveA cpGrooveJointSetGrooveA

{# fun unsafe w_cpGrooveJointGetGrooveB {`Constraint', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpGrooveJointSetGrooveB {`Constraint', with* %`Vect'} -> `()' #}

-- | Second endpoint of groove (on first body).
grooveJointGrooveB :: Constraint -> StateVar Vect
grooveJointGrooveB = mkStateVar w_cpGrooveJointGetGrooveB cpGrooveJointSetGrooveB

{# fun unsafe w_cpGrooveJointGetAnchorB {`Constraint', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpGrooveJointSetAnchorB {`Constraint', with* %`Vect'} -> `()' #}

-- | Anchor on second body.
grooveJointAnchorB :: Constraint -> StateVar Vect
grooveJointAnchorB = mkStateVar w_cpGrooveJointGetAnchorB cpGrooveJointSetAnchorB

-- | Defined much like a slide joint.
{# fun unsafe cpDampedSpringNew as dampedSpringNew
  { `Body'        -- ^ First body to connect
  , `Body'        -- ^ Second body to connect
  , with* %`Vect' -- ^ First anchor
  , with* %`Vect' -- ^ Second anchor
  , `Double'      -- ^ Distance the spring wants to be
  , `Double'      -- ^ Spring constant (<http://en.wikipedia.org/wiki/Young%27s_modulus Young's modulus>)
  , `Double'      -- ^ How soft to make damping of the spring
  } -> `Constraint' #}

{# fun unsafe w_cpDampedSpringGetAnchorA {`Constraint', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpDampedSpringSetAnchorA {`Constraint', with* %`Vect'} -> `()' #}

-- | Anchor on first body.
dampedSpringAnchorA :: Constraint -> StateVar Vect
dampedSpringAnchorA = mkStateVar w_cpDampedSpringGetAnchorA cpDampedSpringSetAnchorA

{# fun unsafe w_cpDampedSpringGetAnchorB {`Constraint', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpDampedSpringSetAnchorB {`Constraint', with* %`Vect'} -> `()' #}

-- | Anchor on second body.
dampedSpringAnchorB :: Constraint -> StateVar Vect
dampedSpringAnchorB = mkStateVar w_cpDampedSpringGetAnchorB cpDampedSpringSetAnchorB

{# fun unsafe cpDampedSpringGetRestLength {`Constraint'} -> `Double' #}

{# fun unsafe cpDampedSpringSetRestLength {`Constraint', `Double'} -> `()' #}

-- | Desired distance the spring will try to enforce.
dampedSpringRestLength :: Constraint -> StateVar Double
dampedSpringRestLength = mkStateVar cpDampedSpringGetRestLength cpDampedSpringSetRestLength

{# fun unsafe cpDampedSpringGetStiffness {`Constraint'} -> `Double' #}

{# fun unsafe cpDampedSpringSetStiffness {`Constraint', `Double'} -> `()' #}

-- | Spring stiffness
dampedSpringStiffness :: Constraint -> StateVar Double
dampedSpringStiffness = mkStateVar cpDampedSpringGetStiffness cpDampedSpringSetStiffness

{# fun unsafe cpDampedSpringGetDamping {`Constraint'} -> `Double' #}

{# fun unsafe cpDampedSpringSetDamping {`Constraint', `Double'} -> `()' #}

-- | Spring damping
dampedSpringDamping :: Constraint -> StateVar Double
dampedSpringDamping = mkStateVar cpDampedSpringGetDamping cpDampedSpringSetDamping

-- | Create new damped rotary spring constraint
{# fun unsafe cpDampedRotarySpringNew as dampedRotarySpringNew
  { `Body'    -- ^ First body to connect
  , `Body'    -- ^ Second body to connect
  , `Double'  -- ^ Relative angle in radians that the bodies want to have
  , `Double'  -- ^ Spring constant (stiffness)
  , `Double'  -- ^ Spring damping
  } -> `Constraint' #}

{# fun unsafe cpDampedRotarySpringGetRestAngle {`Constraint'} -> `Double' #}

{# fun unsafe cpDampedRotarySpringSetRestAngle {`Constraint', `Double'} -> `()' #}

-- | Set desired angle in radians the spring will try to enforce.
dampedRotarySpringRestAngle :: Constraint -> StateVar Double
dampedRotarySpringRestAngle = mkStateVar cpDampedRotarySpringGetRestAngle cpDampedRotarySpringSetRestAngle

{# fun unsafe cpDampedRotarySpringGetStiffness {`Constraint'} -> `Double' #}

{# fun unsafe cpDampedRotarySpringSetStiffness {`Constraint', `Double'} -> `()' #}

-- | Spring stiffness.
dampedRotarySpringStiffness :: Constraint -> StateVar Double
dampedRotarySpringStiffness = mkStateVar cpDampedRotarySpringGetStiffness cpDampedRotarySpringSetStiffness

{# fun unsafe cpDampedRotarySpringGetDamping {`Constraint'} -> `Double' #}

{# fun unsafe cpDampedRotarySpringSetDamping {`Constraint', `Double'} -> `()' #}

-- | Spring damping.
dampedRotarySpringDamping :: Constraint -> StateVar Double
dampedRotarySpringDamping = mkStateVar cpDampedRotarySpringGetDamping cpDampedRotarySpringSetDamping

-- | Create new rotation limiting joint
{# fun unsafe cpRotaryLimitJointNew as rotaryLimitJointNew
  { `Body'    -- ^ First body to connect
  , `Body'    -- ^ Second body to connect
  , `Double'  -- ^ Minimum angle in radians the joint will enforce
  , `Double'  -- ^ Maximum angle in radians the joint will enforce
  } -> `Constraint' #}

{# fun unsafe cpRotaryLimitJointGetMin {`Constraint'} -> `Double' #}

{# fun unsafe cpRotaryLimitJointSetMin {`Constraint', `Double'} -> `()' #}

-- | Minimum angle in radians the joint will try to enforce.
rotaryLimitJointMin :: Constraint -> StateVar Double
rotaryLimitJointMin = mkStateVar cpRotaryLimitJointGetMin cpRotaryLimitJointSetMin

{# fun unsafe cpRotaryLimitJointGetMax {`Constraint'} -> `Double' #}

{# fun unsafe cpRotaryLimitJointSetMax {`Constraint', `Double'} -> `()' #}

-- | Maximum angle in radians the joint will try to enforce.
rotaryLimitJointMax :: Constraint -> StateVar Double
rotaryLimitJointMax = mkStateVar cpRotaryLimitJointGetMax cpRotaryLimitJointSetMax

-- | Allocate and initialize a ratchet joint.
{# fun unsafe cpRatchetJointNew as ratchetJointNew
  { `Body'    -- ^ First body to connect
  , `Body'    -- ^ Second body to connect
  , `Double'  -- ^ The initial offset to use when deciding where the ratchet angles are.
  , `Double'  -- ^ The distance between “clicks”
  } -> `Constraint' #}

{# fun unsafe cpRatchetJointGetAngle {`Constraint'} -> `Double' #}

{# fun unsafe cpRatchetJointSetAngle {`Constraint', `Double'} -> `()' #}

-- | The angle of the current ratchet tooth.
ratchetJointAngle :: Constraint -> StateVar Double
ratchetJointAngle = mkStateVar cpRatchetJointGetAngle cpRatchetJointSetAngle

{# fun unsafe cpRatchetJointGetPhase {`Constraint'} -> `Double' #}

{# fun unsafe cpRatchetJointSetPhase {`Constraint', `Double'} -> `()' #}

-- | The phase offset of the ratchet.
ratchetJointPhase :: Constraint -> StateVar Double
ratchetJointPhase = mkStateVar cpRatchetJointGetPhase cpRatchetJointSetPhase

{# fun unsafe cpRatchetJointGetRatchet {`Constraint'} -> `Double' #}

{# fun unsafe cpRatchetJointSetRatchet {`Constraint', `Double'} -> `()' #}

-- | The angular distance of each ratchet.
ratchetJointRatchet :: Constraint -> StateVar Double
ratchetJointRatchet = mkStateVar cpRatchetJointGetRatchet cpRatchetJointSetRatchet

-- | Allocate and initialize a gear joint.
{# fun unsafe cpGearJointNew as gearJointNew
  { `Body'    -- ^ First body to connect
  , `Body'    -- ^ Second body to connect
  , `Double'  -- ^ The initial angular offset of the two bodies.
  , `Double'  -- ^ Ratio measures in absolute terms
  } -> `Constraint' #}

{# fun unsafe cpGearJointGetPhase {`Constraint'} -> `Double' #}

{# fun unsafe cpGearJointSetPhase {`Constraint', `Double'} -> `()' #}

-- | Phase offset of the ratchet.
gearJointPhase :: Constraint -> StateVar Double
gearJointPhase = mkStateVar cpGearJointGetPhase cpGearJointSetPhase

{# fun unsafe cpGearJointGetRatio {`Constraint'} -> `Double' #}

{# fun unsafe cpGearJointSetRatio {`Constraint', `Double'} -> `()' #}

-- | Ratio of the ratchet
gearJointRatio :: Constraint -> StateVar Double
gearJointRatio = mkStateVar cpGearJointGetRatio cpGearJointSetRatio

-- | Allocate and initialize a simple motor.
{# fun unsafe cpSimpleMotorNew as simpleMotorNew
  { `Body'    -- ^ First body to connect
  , `Body'    -- ^ Second body to connect
  , `Double'  -- ^ The desired relative angular velocity.
  } -> `Constraint' #}

{# fun unsafe cpSimpleMotorGetRate {`Constraint'} -> `Double' #}

{# fun unsafe cpSimpleMotorSetRate {`Constraint', `Double'} -> `()' #}

-- | Ratio of angular velocities.
simpleMotorRate :: Constraint -> StateVar Double
simpleMotorRate = mkStateVar cpSimpleMotorGetRate cpSimpleMotorSetRate
