module Chiphunk.Low.Arbiter
  ( Arbiter
  , arbiterRestitution
  , arbiterFriction
  , arbiterSurfaceVelocity
  , arbiterUserData
  , arbiterCount
  , arbiterNormal
  , arbiterPointA
  , arbiterPointB
  , arbiterDepth
  , arbiterIsFirstContact
  , arbiterIsRemoval
  , arbiterShapes
  , arbiterBodies
  , arbiterCallWildcardBeginA
  , arbiterCallWildcardBeginB
  , arbiterCallWildcardPreSolveA
  , arbiterCallWildcardPreSolveB
  , arbiterCallWildcardPostSolveA
  , arbiterCallWildcardPostSolveB
  , arbiterCallWildcardSeparateA
  , arbiterCallWildcardSeparateB
  ) where

import Data.StateVar
import Foreign

{# import Chiphunk.Low.Types #}

#include <chipmunk/chipmunk.h>
#include <wrapper.h>

{# fun unsafe cpArbiterGetRestitution {`Arbiter'} -> `Double' #}

{# fun unsafe cpArbiterSetRestitution {`Arbiter', `Double'} -> `()' #}

-- | The calculated elasticity for this collision pair.
-- Setting the value in a preSolve() callback will override the value calculated by the space.
-- The default calculation multiplies the elasticity of the two shapes together.
arbiterRestitution :: Arbiter -> StateVar Double
arbiterRestitution = mkStateVar cpArbiterGetRestitution cpArbiterSetRestitution

{# fun unsafe cpArbiterGetFriction {`Arbiter'} -> `Double' #}

{# fun unsafe cpArbiterSetFriction {`Arbiter', `Double'} -> `()' #}

-- | The calculated friction for this collision pair.
-- Setting the value in a preSolve() callback will override the value calculated by the space.
-- The default calculation multiplies the friction of the two shapes together.
arbiterFriction :: Arbiter -> StateVar Double
arbiterFriction = mkStateVar cpArbiterGetFriction cpArbiterSetFriction

{# fun unsafe w_cpArbiterGetSurfaceVelocity {`Arbiter', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpArbiterSetSurfaceVelocity {`Arbiter', with* %`Vect'} -> `()' #}

-- | The calculated surface velocity for this collision pair.
-- Setting the value in a preSolve() callback will override the value calculated by the space.
-- The default calculation subtracts the surface velocity of the second shape
-- from the first and then projects that onto the tangent of the collision.
-- This is so that only friction is affected by default calculation.
--
-- Using a custom calculation, you can make something that responds like a pinball bumper,
-- or where the surface velocity is dependent on the location of the contact point.
arbiterSurfaceVelocity :: Arbiter -> StateVar Vect
arbiterSurfaceVelocity = mkStateVar w_cpArbiterGetSurfaceVelocity cpArbiterSetSurfaceVelocity

{# fun unsafe cpArbiterGetUserData {`Arbiter'} -> `DataPtr' #}

{# fun unsafe cpArbiterSetUserData {`Arbiter', `DataPtr'} -> `()' #}

-- | A user definable context pointer.
-- The value will persist until just after the separate callback is called for the pair.
--
-- __Note__: If you need to clean up this pointer, you should implement the separate callback to do it.
-- Also be careful when destroying the space as there may be active collisions still.
-- In order to trigger the separate callbacks and clean up your data,
-- you’ll need to remove all the shapes from the space before disposing of it.
-- This is something I’d suggest doing anyway.
-- See ChipmunkDemo.c:ChipmunkDemoFreeSpaceChildren() for an example of how to do it easily.
arbiterUserData :: Arbiter -> StateVar DataPtr
arbiterUserData = mkStateVar cpArbiterGetUserData cpArbiterSetUserData

{# fun unsafe cpArbiterGetCount {`Arbiter'} -> `Int' #}

-- | The number of contacts tracked by this arbiter.
-- For the forseeable future, the maximum number of contacts will be two.
arbiterCount :: Arbiter -> GettableStateVar Int
arbiterCount = makeGettableStateVar . cpArbiterGetCount

{# fun unsafe w_cpArbiterGetNormal {`Arbiter', alloca- `Vect' peek*} -> `()' #}

-- | Collision normal in a specific point tracked by this collision.
arbiterNormal :: Arbiter -> GettableStateVar Vect
arbiterNormal = makeGettableStateVar . w_cpArbiterGetNormal

{# fun unsafe w_cpArbiterGetPointA {`Arbiter', `Int', alloca- `Vect' peek*} -> `()' #}

-- | Collision point of a specific point on first body.
arbiterPointA :: Arbiter -> Int -> GettableStateVar Vect
arbiterPointA arbiter = makeGettableStateVar . w_cpArbiterGetPointA arbiter

{# fun unsafe w_cpArbiterGetPointB {`Arbiter', `Int', alloca- `Vect' peek*} -> `()' #}

-- | Collision point of a specific point on second body.
arbiterPointB :: Arbiter -> Int -> GettableStateVar Vect
arbiterPointB arbiter = makeGettableStateVar . w_cpArbiterGetPointB arbiter

{# fun unsafe cpArbiterGetDepth {`Arbiter', `Int'} -> `Double' #}

-- | Penetration depth of a collision point.
arbiterDepth :: Arbiter -> Int -> GettableStateVar Double
arbiterDepth arbiter = makeGettableStateVar . cpArbiterGetDepth arbiter

-- | Returns true if this is the first step the two shapes started touching. This can be useful for sound effects
-- for instance. If its the first frame for a certain collision, check the energy of the collision in a postStep
-- callbock and use that to determine the volume of a sound effect to play.
{# fun unsafe cpArbiterIsFirstContact as arbiterIsFirstContact {`Arbiter'} -> `Bool' #}

-- | Returns 'True' during a separate callback if the callback was invoked due to an object removal.
{# fun unsafe cpArbiterIsRemoval as arbiterIsRemoval {`Arbiter'} -> `Bool' #}

{# fun unsafe cpArbiterGetShapes
  { `Arbiter'
  , alloca- `Shape' peek*
  , alloca- `Shape' peek*
  } -> `()' #}

-- | The colliding shapes in the order that they were defined in the collision handler
-- associated with this arbiter.
-- If you defined the handler as cpSpaceAddCollisionHandler(space, 1, 2, ...),
-- you you will find that a->collision_type == 1 and b->collision_type == 2.
arbiterShapes :: Arbiter -> GettableStateVar (Shape, Shape)
arbiterShapes = makeGettableStateVar . cpArbiterGetShapes

{# fun unsafe cpArbiterGetBodies
  { `Arbiter'
  , alloca- `Body' peek*
  , alloca- `Body' peek*
  } -> `()' #}

-- | The colliding bodies in the order that they were defined in the collision handler
-- associated with this arbiter.
-- If you defined the handler as cpSpaceAddCollisionHandler(space, 1, 2, ...),
-- you you will find that a->collision_type == 1 and b->collision_type == 2.
arbiterBodies :: Arbiter -> GettableStateVar (Body, Body)
arbiterBodies = makeGettableStateVar . cpArbiterGetBodies

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
