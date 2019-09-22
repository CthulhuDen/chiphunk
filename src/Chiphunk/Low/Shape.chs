-- | Description: Shapes manipulations
-- Module provides access to the shapes which define collisions of rigid bodies.
module Chiphunk.Low.Shape
  ( Shape
  , shapeBody
  , shapeBB
  , shapeSensor
  , shapeElasticity
  , shapeFriction
  , shapeSurfaceVelocity
  , shapeCollisionType
  , shapeMass
  , shapeDensity
  , ShapeFilter (..)
  , ShapeFilterPtr
  , shapeFilter
  , shapeSpace
  , shapeUserData
  , shapeFree
  , shapeCacheBB
  , shapeUpdate
  , circleShapeNew
  , segmentShapeNew
  , segmentShapeNeighbors
  , polyShapeNew
  , polyShapeNewRaw
  , boxShapeNew
  , boxShapeNew2
  ) where

import Foreign

import Chiphunk.Low.Internal
import Data.StateVar

{# import Chiphunk.Low.Types #}

#include <chipmunk/chipmunk.h>
#include <wrapper.h>

{# fun unsafe cpShapeGetBody {`Shape'} -> `Body' #}

{# fun unsafe cpShapeSetBody {`Shape', `Body'} -> `()' #}

-- | The rigid body the shape is attached to.
-- Can only be set when the shape is not added to a space.
shapeBody :: Shape -> StateVar Body
shapeBody = mkStateVar cpShapeGetBody cpShapeSetBody

{# fun unsafe w_cpShapeGetBB {`Shape', alloca- `BB' peek*} -> `()' #}

-- | The bounding box of the shape.
-- Only guaranteed to be valid after 'shapeCacheBB' or 'spaceStep' is called.
-- Moving a body that a shape is connected to does not update its bounding box.
-- For shapes used for queries that aren’t attached to bodies, you can also use 'shapeUpdate'.
shapeBB :: Shape -> GettableStateVar BB
shapeBB = makeGettableStateVar . w_cpShapeGetBB

{# fun unsafe cpShapeGetSensor {`Shape'} -> `Bool' #}

{# fun unsafe cpShapeSetSensor {`Shape', `Bool'} -> `()' #}

-- | A boolean value if this shape is a sensor or not.
-- Sensors only call collision callbacks, and never generate real collisions.
shapeSensor :: Shape -> StateVar Bool
shapeSensor = mkStateVar cpShapeGetSensor cpShapeSetSensor

{# fun unsafe cpShapeGetElasticity {`Shape'} -> `Double' #}

{# fun unsafe cpShapeSetElasticity {`Shape', `Double'} -> `()' #}

-- | Elasticity of the shape.
-- A value of 0.0 gives no bounce, while a value of 1.0 will give a “perfect” bounce.
-- However due to inaccuracies in the simulation using 1.0 or greater is not recommended however.
--
-- The elasticity for a collision is found by multiplying the elasticity of the individual shapes together.
shapeElasticity :: Shape -> StateVar Double
shapeElasticity = mkStateVar cpShapeGetElasticity cpShapeSetElasticity

{# fun unsafe cpShapeGetFriction {`Shape'} -> `Double' #}

{# fun unsafe cpShapeSetFriction {`Shape', `Double'} -> `()' #}

-- | Friction coefficient.
-- Chipmunk uses the Coulomb friction model, a value of 0.0 is frictionless.
--
-- The friction for a collision is found by multiplying the friction of the individual shapes together.
-- <http://www.roymech.co.uk/Useful_Tables/Tribology/co_of_frict.htm Table of friction coefficients.>
shapeFriction :: Shape -> StateVar Double
shapeFriction = mkStateVar cpShapeGetFriction cpShapeSetFriction

{# fun unsafe w_cpShapeGetSurfaceVelocity {`Shape', alloca- `Vect' peek*} -> `()' #}

{# fun unsafe cpShapeSetSurfaceVelocity {`Shape', with* %`Vect'} -> `()' #}

-- | The surface velocity of the object.
-- Useful for creating conveyor belts or players that move around.
-- This value is only used when calculating friction, not resolving the collision.
shapeSurfaceVelocity :: Shape -> StateVar Vect
shapeSurfaceVelocity = mkStateVar w_cpShapeGetSurfaceVelocity cpShapeSetSurfaceVelocity

{# fun unsafe cpShapeGetCollisionType {`Shape'} -> `CollisionType' fromIntegral #}

{# fun unsafe cpShapeSetCollisionType {`Shape', fromIntegral `CollisionType'} -> `()' #}

-- | Collision type of this shape.
-- | You can assign types to Chipmunk collision shapes
-- that trigger callbacks when objects of certain types touch.
-- See the callbacks section for more information.
shapeCollisionType :: Shape -> StateVar CollisionType
shapeCollisionType = mkStateVar cpShapeGetCollisionType cpShapeSetCollisionType

{# fun unsafe cpShapeGetMass {`Shape'} -> `Double' #}

{# fun unsafe cpShapeSetMass {`Shape', `Double'} -> `()' #}

shapeMass :: Shape -> StateVar Double
shapeMass = mkStateVar cpShapeGetMass cpShapeSetMass

{# fun unsafe cpShapeGetDensity {`Shape'} -> `Double' #}

{# fun unsafe cpShapeSetDensity {`Shape', `Double'} -> `()' #}

shapeDensity :: Shape -> StateVar Double
shapeDensity = mkStateVar cpShapeGetDensity cpShapeSetDensity

-- | Fast collision filtering type that is used to determine if two objects collide
-- before calling collision or query callbacks.
data ShapeFilter = ShapeFilter
  { sfGroup :: !WordPtr
  , sfCategories :: !Word32
  , sfMask :: !Word32
  } deriving Show

instance Storable ShapeFilter where
  sizeOf _    = {# sizeof cpShapeFilter #}
  alignment _ = {# alignof cpShapeFilter #}
  poke p (ShapeFilter g c m) = do
    {# set cpShapeFilter->group #} p      $ fromIntegral g
    {# set cpShapeFilter->categories #} p $ fromIntegral c
    {# set cpShapeFilter->mask #} p       $ fromIntegral m
  peek p = ShapeFilter <$> (fromIntegral <$> {# get cpShapeFilter->group #} p)
                       <*> (fromIntegral <$> {# get cpShapeFilter->categories #} p)
                       <*> (fromIntegral <$> {# get cpShapeFilter->mask #} p)

-- | Pointer to 'ShapeFilter'
{# pointer *cpShapeFilter as ShapeFilterPtr -> ShapeFilter #}

{# fun unsafe w_cpShapeGetFilter {`Shape', alloca- `ShapeFilter' peek*} -> `()' #}

{# fun unsafe cpShapeSetFilter {`Shape', with* %`ShapeFilter'} -> `()' #}

-- | The collision filter for this shape. See Filtering Collisions for more information.
shapeFilter :: Shape -> StateVar ShapeFilter
shapeFilter = mkStateVar w_cpShapeGetFilter cpShapeSetFilter

{# fun unsafe cpShapeGetSpace {`Shape'} -> `Space' #}

-- | The 'Space' that @shape@ has been added to.
shapeSpace :: Shape -> GettableStateVar Space
shapeSpace = makeGettableStateVar . cpShapeGetSpace

{# fun unsafe cpShapeGetUserData {`Shape'} -> `DataPtr' #}

{# fun unsafe cpShapeSetUserData {`Shape', `DataPtr'} -> `()' #}

-- | A user definable data pointer.
-- If you set this to point at the game object the shapes is for,
-- then you can access your game object from Chipmunk callbacks.
shapeUserData :: Shape -> StateVar DataPtr
shapeUserData = mkStateVar cpShapeGetUserData cpShapeSetUserData

-- | Deallocates shape.
{# fun cpShapeFree as shapeFree {`Shape'} -> `()' #}
-- no "unsafe" qualifier because I think it may trigger separte callbacks

-- | Synchronizes @shape@ with the body its attached to.
{# fun unsafe w_cpShapeCacheBB as shapeCacheBB
  { `Shape'            -- ^ shape
  , alloca- `BB' peek*
  } -> `()' #}

-- | Sets the position and rotation of the shape
{# fun unsafe w_cpShapeUpdate as shapeUpdate
  { `Shape'            -- ^ @shape@
  , with* %`Transform'
  , alloca- `BB' peek*
  } -> `()' #}

-- | Create new circle-like shape.
{# fun unsafe cpCircleShapeNew as circleShapeNew
  { `Body'        -- ^ The body to attach the circle to.
  , `Double'      -- ^ Radius of the circle.
  , with* %`Vect' -- ^ Offset from the body's center of gravity in body local coordinates.
  } -> `Shape' #}

-- | Create new segment-shaped shape.
{# fun unsafe cpSegmentShapeNew as segmentShapeNew
  { `Body'        -- ^ The body to attach the segment to.
  , with* %`Vect' -- ^ One endpoint.
  , with* %`Vect' -- ^ Another endpoint.
  , `Double'      -- ^ The thickness of the segment.
  } -> `Shape' #}

{# fun unsafe cpSegmentShapeSetNeighbors {`Shape', with* %`Vect', with* %`Vect'} -> `()' #}

-- | When you have a number of segment shapes that are all joined together,
-- things can still collide with the “cracks” between the segments.
-- By setting the neighbor segment endpoints
-- you can tell Chipmunk to avoid colliding with the inner parts of the crack.
segmentShapeNeighbors :: Shape -> SettableStateVar (Vect, Vect)
segmentShapeNeighbors shape =
  makeSettableStateVar $ \(v1, v2) ->
    cpSegmentShapeSetNeighbors shape v1 v2

-- | A convex hull will be calculated from the vertexes automatically.
-- The polygon shape will be created with a radius, increasing the size of the shape.
{# fun unsafe cpPolyShapeNew as polyShapeNew
  { `Body'              -- ^ The body to attach the poly to.
  , withList* `[Vect]'& -- ^ The array of 'Vect' structs.
  , with* %`Transform'  -- ^ The transform that will be applied to every vertex.
  , `Double'            -- ^ Radius.
  } -> `Shape' #}

-- | Alternate constructors for poly shapes. This version does not apply a transform nor does it create a convex hull.
-- Verticies must be provided with a counter-clockwise winding.
{# fun unsafe cpPolyShapeNewRaw as polyShapeNewRaw {`Body', withList* `[Vect]'&, `Double'} -> `Shape' #}

-- | Createa box shape from dimensions.
{# fun unsafe cpBoxShapeNew as boxShapeNew
  { `Body'    -- ^ The body to attach to
  , `Double'  -- ^ Box width
  , `Double'  -- ^ Box height
  , `Double'  -- ^ Radius
  } -> `Shape' #}

-- | Alternative to 'boxShapeNew' using 'BB' to set size.
{# fun unsafe cpBoxShapeNew2 as boxShapeNew2
  { `Body'      -- ^ The body to attach to
  , with* %`BB' -- ^ Shape size
  , `Double'    -- ^ Radius
  } -> `Shape' #}
