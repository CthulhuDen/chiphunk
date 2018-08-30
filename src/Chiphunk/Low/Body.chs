module Chiphunk.Low.Body where

import Foreign

{# import Chiphunk.Low.Types #}

#include <chipmunk/chipmunk.h>
#include <wrapper.h>

{# fun unsafe cpBodyNew as bodyNew {`Double', `Double'} -> `BodyPtr' #}

{# fun unsafe cpBodyNewKinematic as bodyNewKinematic {} -> `BodyPtr' #}

{# fun unsafe cpBodyNewStatic as bodyNewStatic {} -> `BodyPtr' #}

{# fun unsafe cpBodyFree as bodyFree {`BodyPtr'} -> `()' #}

{# fun unsafe cpBodyGetType as bodyGetType {`BodyPtr'} -> `BodyType' #}

{# fun unsafe cpBodySetType as bodySetType {`BodyPtr', `BodyType'} -> `()' #}

{# fun unsafe cpBodyGetMass as bodyGetMass {`BodyPtr'} -> `Double' #}

{# fun unsafe cpBodySetMass as bodySetMass {`BodyPtr', `Double'} -> `()' #}

{# fun unsafe cpBodyGetMoment as bodyGetMoment {`BodyPtr'} -> `Double' #}

{# fun unsafe cpBodySetMoment as bodySetMoment {`BodyPtr', `Double'} -> `()' #}

{# fun unsafe w_cpBodyGetPosition as bodyGetPosition {`BodyPtr', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpBodySetPosition as bodySetPosition {`BodyPtr', with* %`Vect'} -> `()' #}

{# fun unsafe w_cpBodyGetCenterOfGravity as bodyGetCenterOfGravity {`BodyPtr', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpBodySetCenterOfGravity as bodySetCenterOfGravity {`BodyPtr', with* %`Vect'} -> `()' #}

{# fun unsafe w_cpBodyGetVelocity as bodyGetVelocity {`BodyPtr', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpBodySetVelocity as bodySetVelocity {`BodyPtr', with* %`Vect'} -> `()' #}

{# fun unsafe w_cpBodyGetForce as bodyGetForce {`BodyPtr', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpBodySetForce as bodySetForce {`BodyPtr', with* %`Vect'} -> `()' #}

{# fun unsafe cpBodyGetAngle as bodyGetAngle {`BodyPtr'} -> `Double' #}

{# fun unsafe cpBodySetAngle as bodySetAngle {`BodyPtr', `Double'} -> `()' #}

{# fun unsafe cpBodyGetAngularVelocity as bodyGetAngularVelocity {`BodyPtr'} -> `Double' #}

{# fun unsafe cpBodySetAngularVelocity as bodySetAngularVelocity {`BodyPtr', `Double'} -> `()' #}

{# fun unsafe cpBodyGetTorque as bodyGetTorque {`BodyPtr'} -> `Double' #}

{# fun unsafe cpBodySetTorque as bodySetTorque {`BodyPtr', `Double'} -> `()' #}

{# fun unsafe w_cpBodyGetRotation as bodyGetRotation {`BodyPtr', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpBodyGetSpace as bodyGetSpace {`BodyPtr'} -> `SpacePtr' #}

{# fun unsafe cpBodyGetUserData as bodyGetUserData {`BodyPtr'} -> `DataPtr' #}

{# fun unsafe cpBodySetUserData as bodySetUserData {`BodyPtr', `DataPtr'} -> `()' #}

{# fun unsafe w_cpBodyLocalToWorld as bodyLocalToWorld {`BodyPtr', with* %`Vect', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe w_cpBodyWorldToLocal as bodyWorldToLocal {`BodyPtr', with* %`Vect', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe w_cpBodyGetVelocityAtWorldPoint as bodyGetVelocityAtWorldPoint
  {`BodyPtr', with* %`Vect', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe w_cpBodyGetVelocityAtLocalPoint as bodyGetVelocityAtLocalPoint
  {`BodyPtr', with* %`Vect', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpBodyApplyForceAtWorldPoint as bodyApplyForceAtWorldPoint
  {`BodyPtr', with* %`Vect', with* %`Vect'} -> `()' #}

{# fun unsafe cpBodyApplyForceAtLocalPoint as bodyApplyForceAtLocalPoint
  {`BodyPtr', with* %`Vect', with* %`Vect'} -> `()' #}

{# fun unsafe cpBodyApplyImpulseAtWorldPoint as bodyApplyImpulseAtWorldPoint
  {`BodyPtr', with* %`Vect', with* %`Vect'} -> `()' #}

{# fun unsafe cpBodyApplyImpulseAtLocalPoint as bodyApplyImpulseAtLocalPoint
  {`BodyPtr', with* %`Vect', with* %`Vect'} -> `()' #}

{# fun unsafe cpBodyIsSleeping as bodyIsSleeping {`BodyPtr'} -> `Bool' #}

{# fun unsafe cpBodyActivate as bodyActivate {`BodyPtr'} -> `()' #}

{# fun unsafe cpBodySleep as bodySleep {`BodyPtr'} -> `()' #}

{# fun unsafe cpBodyActivateStatic as bodyActivateStatic {`BodyPtr', `ShapePtr'} -> `()' #}

{# fun unsafe cpBodySleepWithGroup as bodySleepWithGroup {`BodyPtr', `BodyPtr'} -> `()' #}
