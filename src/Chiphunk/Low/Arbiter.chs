module Chiphunk.Low.Arbiter
  ( Arbiter
  , arbiterGetRestitution
  , arbiterSetRestitution
  , arbiterGetFriction
  , arbiterSetFriction
  , arbiterGetSurfaceVelocity
  , arbiterSetSurfaceVelocity
  , arbiterGetUserData
  , arbiterSetUserData
  , arbiterGetCount
  , arbiterGetNormal
  , arbiterGetPointA
  , arbiterGetPointB
  , arbiterGetDepth
  , arbiterIsFirstContact
  , arbiterIsRemoval
  , arbiterGetShapes
  , arbiterGetBodies
  , arbiterCallWildcardBeginA
  , arbiterCallWildcardBeginB
  , arbiterCallWildcardPreSolveA
  , arbiterCallWildcardPreSolveB
  , arbiterCallWildcardPostSolveA
  , arbiterCallWildcardPostSolveB
  , arbiterCallWildcardSeparateA
  , arbiterCallWildcardSeparateB
  ) where

import Foreign

{# import Chiphunk.Low.Types #}

#include <chipmunk/chipmunk.h>
#include <wrapper.h>

-- | Get the calculated elasticity for this collision pair.
-- The default calculation multiplies the elasticity of the two shapes together.
{# fun unsafe cpArbiterGetRestitution as arbiterGetRestitution {`Arbiter'} -> `Double' #}

-- | Set the overridden elasticity for this collision pair.
-- Setting the value in a preSolve callback will override the value calculated by the space.
{# fun unsafe cpArbiterSetRestitution as arbiterSetRestitution {`Arbiter', `Double'} -> `()' #}

-- | Get the calculated friction for this collision pair.
-- The default calculation multiplies the friction of the two shapes together.
{# fun unsafe cpArbiterGetFriction as arbiterGetFriction {`Arbiter'} -> `Double' #}

-- | Set friction for this collision pair.
-- Setting the value in a preSolve callback will override the value calculated by the space.
{# fun unsafe cpArbiterSetFriction as arbiterSetFriction {`Arbiter', `Double'} -> `()' #}

-- | Get the calculated surface velocity for this collision pair. The default calculation
-- subtracts the surface velocity of the second shape from the first
-- and then projects that onto the tangent of the collision.
-- This is so that only friction is affected by default calculation.
{# fun unsafe w_cpArbiterGetSurfaceVelocity as arbiterGetSurfaceVelocity {`Arbiter', alloca- `Vect' peek*} -> `()' #}

-- | Override calculated surface velocity for this collision pair. Setting the value in a preSolve callback
-- will override the value calculated by the space. Using a custom calculation, you can make something
-- that responds like a pinball bumper, or where the surface velocity is dependent on the location of the contact point.
{# fun unsafe cpArbiterSetSurfaceVelocity as arbiterSetSurfaceVelocity {`Arbiter', with* %`Vect'} -> `()' #}

-- | Get a user definable context pointer.
{# fun unsafe cpArbiterGetUserData as arbiterGetUserData {`Arbiter'} -> `DataPtr' #}

-- | Set a user definable context pointer. The value will persist until just after the separate callback
-- is called for the pair.
--
-- __Note__: If you need to clean up this pointer, you should implement the separate callback to do it.
-- Also be careful when destroying the space as there may be active collisions still.
-- In order to trigger the separate callbacks and clean up your data, you’ll need to remove all the shapes
-- from the space before disposing of it. This is something I’d suggest doing anyway.
-- See ChipmunkDemo.c:ChipmunkDemoFreeSpaceChildren() for an example of how to do it easily.
{# fun unsafe cpArbiterSetUserData as arbiterSetUserData {`Arbiter', `DataPtr'} -> `()' #}

-- | Get the number of contacts tracked by this arbiter.
-- For the forseeable future, the maximum number of contacts will be two.
{# fun unsafe cpArbiterGetCount as arbiterGetCount {`Arbiter'} -> `Int' #}

-- | Get collision normal in a specific point tracked by this collision.
{# fun unsafe w_cpArbiterGetNormal as arbiterGetNormal {`Arbiter', alloca- `Vect' peek*} -> `()' #}

-- | Get collision point of a specific point on first body.
{# fun unsafe w_cpArbiterGetPointA as arbiterGetPointA {`Arbiter', `Int', alloca- `Vect' peek*} -> `()' #}

-- | Get collision point of a specific point on second body.
{# fun unsafe w_cpArbiterGetPointB as arbiterGetPointB {`Arbiter', `Int', alloca- `Vect' peek*} -> `()' #}

-- | Get penetration depth of a collision point.
{# fun unsafe cpArbiterGetDepth as arbiterGetDepth {`Arbiter', `Int'} -> `Double' #}

-- | Returns true if this is the first step the two shapes started touching. This can be useful for sound effects
-- for instance. If its the first frame for a certain collision, check the energy of the collision in a postStep
-- callbock and use that to determine the volume of a sound effect to play.
{# fun unsafe cpArbiterIsFirstContact as arbiterIsFirstContact {`Arbiter'} -> `Bool' #}

-- | Returns 'True' during a separate callback if the callback was invoked due to an object removal.
{# fun unsafe cpArbiterIsRemoval as arbiterIsRemoval {`Arbiter'} -> `Bool' #}

-- | Get the colliding shapes in the order that they were defined in the collision handler associated with this arbiter.
-- If you defined the handler as cpSpaceAddCollisionHandler(space, 1, 2, ...), you you will find that
-- a->collision_type == 1 and b->collision_type == 2.
{# fun unsafe cpArbiterGetShapes as arbiterGetShapes
  { `Arbiter'
  , alloca- `Shape' peek*
  , alloca- `Shape' peek*
  } -> `()' #}

-- | Get the colliding bodies in the order that they were defined in the collision handler associated with this arbiter.
-- If you defined the handler as cpSpaceAddCollisionHandler(space, 1, 2, ...), you you will find that
-- a->collision_type == 1 and b->collision_type == 2.
{# fun unsafe cpArbiterGetBodies as arbiterGetBodies
  { `Arbiter'
  , alloca- `Body' peek*
  , alloca- `Body' peek*
  } -> `()' #}

-- | Run begin wildcard callback for first body.
{# fun cpArbiterCallWildcardBeginA as arbiterCallWildcardBeginA {`Arbiter', `Space'} -> `Bool' #}

-- | Run begin wildcard callback for second body.
{# fun cpArbiterCallWildcardBeginB as arbiterCallWildcardBeginB {`Arbiter', `Space'} -> `Bool' #}

-- | Run preSolve wildcard callback for first body.
{# fun cpArbiterCallWildcardPreSolveA as arbiterCallWildcardPreSolveA {`Arbiter', `Space'} -> `Bool' #}

-- | Run preSolve wildcard callback for second body.
{# fun cpArbiterCallWildcardPreSolveB as arbiterCallWildcardPreSolveB {`Arbiter', `Space'} -> `Bool' #}

-- | Run postSolve wildcard callback for first body.
{# fun cpArbiterCallWildcardPostSolveA as arbiterCallWildcardPostSolveA {`Arbiter', `Space'} -> `()' #}

-- | Run postSolve wildcard callback for second body.
{# fun cpArbiterCallWildcardPostSolveB as arbiterCallWildcardPostSolveB {`Arbiter', `Space'} -> `()' #}

-- | Run separate wildcard callback for first body.
{# fun cpArbiterCallWildcardSeparateA as arbiterCallWildcardSeparateA {`Arbiter', `Space'} -> `()' #}

-- | Run separate wildcard callback for second body.
{# fun cpArbiterCallWildcardSeparateB as arbiterCallWildcardSeparateB {`Arbiter', `Space'} -> `()' #}
