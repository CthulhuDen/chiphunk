{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Description: Basic Chipmunk data types.
-- Module provides basic Chipmunk data types.
module Chiphunk.Low.Types
  ( Vect (..)
  , VectPtr
  , BB (..)
  , BBPtr
  , DataPtr
  , Body (..)
  , BodyType (..)
  , Space (..)
  , Shape (..)
  , Constraint (..)
  , Arbiter (..)
  , Transform (..)
  , TransformPtr
  , CollisionType
  , CPBool
  ) where

import Data.Cross
import Data.VectorSpace
import Foreign

#include <chipmunk/chipmunk.h>

-- | 2D vector packed into a struct.
data Vect = Vect
  { vX :: !Double, vY :: !Double
  } deriving (Eq, Show)

instance AdditiveGroup Vect where
  zeroV = Vect 0 0
  negateV (Vect x y) = Vect (-x) (-y)
  Vect x1 y1 ^+^ Vect x2 y2 = Vect (x1 + x2) (y1 + y2)
  Vect x1 y1 ^-^ Vect x2 y2 = Vect (x1 - x2) (y1 - y2)

instance VectorSpace Vect where
  type Scalar Vect = Double
  f *^ Vect x y = Vect (f * x) (f * y)

instance InnerSpace Vect where
  Vect x1 y1 <.> Vect x2 y2 = x1 * x2 + y1 * y2

instance HasCross2 Vect where
  cross2 (Vect x y) = Vect (-y) x

instance Storable Vect where
  sizeOf _    = {# sizeof cpVect #}
  alignment _ = {# alignof cpVect #}
  poke p (Vect x y) = do
    {# set cpVect->x #} p $ realToFrac x
    {# set cpVect->y #} p $ realToFrac y
  peek p = Vect <$> (realToFrac <$> {# get cpVect->x #} p)
                <*> (realToFrac <$> {# get cpVect->y #} p)

-- | Pointer to vector.
{# pointer *cpVect as VectPtr -> Vect #}

-- | Simple bounding box struct. Stored as left, bottom, right, top values.
data BB = BB
  { bbL :: !Double, bbB :: !Double, bbR :: !Double, bbT :: !Double
  } deriving (Show)

instance Storable BB where
  sizeOf _    = {# sizeof cpBB #}
  alignment _ = {# alignof cpBB #}
  poke p (BB l b r t) = do
    {# set cpBB->l #} p $ realToFrac l
    {# set cpBB->b #} p $ realToFrac b
    {# set cpBB->r #} p $ realToFrac r
    {# set cpBB->t #} p $ realToFrac t
  peek p = BB <$> (realToFrac <$> {# get cpBB->l #} p)
              <*> (realToFrac <$> {# get cpBB->b #} p)
              <*> (realToFrac <$> {# get cpBB->r #} p)
              <*> (realToFrac <$> {# get cpBB->t #} p)

-- | Pointer to bounding box.
{# pointer *cpBB as BBPtr -> BB #}

-- | Pointer to user data.
{# pointer cpDataPointer as DataPtr #}

-- | Rigid body somewhere in C code.
{# pointer *cpBody as Body newtype #}

-- | Chipmunk supports three different types of bodies with unique behavioral and performance characteristics.
data BodyType =
    BodyTypeDynamic
    -- ^ Dynamic bodies are the default body type.
    -- They react to collisions, are affected by forces and gravity, and have a finite amount of mass.
    -- These are the type of bodies that you want the physics engine to simulate for you.
    -- Dynamic bodies interact with all types of bodies and can generate collision callbacks.
  | BodyTypeKimenatic
    -- ^ Kinematic bodies are bodies that are controlled from your code instead of inside the physics engine.
    -- They arent affected by gravity
    -- and they have an infinite amount of mass so they don’t react to collisions or forces with other bodies.
    -- Kinematic bodies are controlled by setting their velocity, which will cause them to move.
    -- Good examples of kinematic bodies might include things like moving platforms.
    -- Objects that are touching or jointed to a kinematic body are never allowed to fall asleep.
  | BodyTypeStatic
    -- ^ Static bodies are bodies that never (or rarely) move.
    -- Using static bodies for things like terrain offers a big performance boost over other body types —
    -- because Chipmunk doesn’t need to check for collisions between static objects
    -- and it never needs to update their collision information.
    -- Additionally, because static bodies don’t move,
    -- Chipmunk knows it’s safe to let objects that are touching or jointed to them fall asleep.
    -- Generally all of your level geometry will be attached to a static body
    -- except for things like moving platforms or doors.
    -- Every space provide a built-in static body for your convenience.
    -- Static bodies can be moved, but there is a performance penalty as the collision information is recalculated.
    -- There is no penalty for having multiple static bodies, and it can be useful for simplifying your code
    -- by allowing different parts of your static geometry to be initialized or moved separately.

{# enum cpBodyType as BodyType nocode
  { CP_BODY_TYPE_DYNAMIC as BodyTypeDynamic
  , CP_BODY_TYPE_KINEMATIC as BodyTypeKimenatic
  , CP_BODY_TYPE_STATIC as BodyTypeStatic
  } #}

deriving instance Show BodyType

-- | Spaces in Chipmunk are the basic unit of simulation. You add rigid bodies, shapes, and constraints to the space
-- and then step them all forward through time together.
{# pointer *cpSpace as Space newtype #}

-- | There are currently 3 collision shape types:
--
-- * __Circles__: Fastest and simplest collision shape.
--
-- * __Line segments__: Meant mainly as a static shape. Can be beveled in order to give them a thickness.
--
-- * __Convex polygons__: Slowest, but most flexible collision shape.
--
-- You can add as many shapes to a body as you wish. That is why the two types are separate.
--
-- Combining multiple shapes gives you the flexibility to make any object you want
-- as well as providing different areas of the same object with different friction, elasticity or callback values.
{# pointer *cpShape as Shape newtype #}

-- | A constraint is something that describes how two bodies interact with each other. (how they constrain each other)
-- Constraints can be simple joints that allow bodies to pivot around each other like the bones in your body,
-- or they can be more abstract like the gear joint or motors.
{# pointer *cpConstraint as Constraint newtype #}

-- | Chipmunk’s 'Arbiter' struct encapsulates a pair of colliding shapes and all of the data about their collision.
-- 'Arbiter' is created when a collision starts, and persist until those shapes are no longer colliding.
--
-- Why are they called arbiters? The short answer is that I kept using the word “arbitrates”
-- to describe the way that collisions were resolved and then I saw that Box2D actually called them arbiters
-- way back in 2006 when I was looking at its solver.
-- An arbiter is like a judge, a person that has authority to settle disputes between two people.
-- It was a fun, fitting name and was shorter to type than CollisionPair which I had been using.
-- It was originally meant to be a private internal structure only, but evolved to be useful from callbacks.
{# pointer *cpArbiter as Arbiter newtype #}

-- | Type used for 2×3 affine transforms in Chipmunk.
data Transform = Transform
  { tA :: !Double, tB :: !Double, tC :: !Double, tD :: !Double, tTx :: !Double, tTy :: !Double
  } deriving Show

instance Storable Transform where
  sizeOf _    = {# sizeof cpTransform #}
  alignment _ = {# alignof cpTransform #}
  poke p (Transform a b c d tx ty) = do
    {# set cpTransform->a #} p  $ realToFrac a
    {# set cpTransform->b #} p  $ realToFrac b
    {# set cpTransform->c #} p  $ realToFrac c
    {# set cpTransform->d #} p  $ realToFrac d
    {# set cpTransform->tx #} p $ realToFrac tx
    {# set cpTransform->ty #} p $ realToFrac ty
  peek p = Transform <$> (realToFrac <$> {# get cpTransform->a #} p)
                     <*> (realToFrac <$> {# get cpTransform->b #} p)
                     <*> (realToFrac <$> {# get cpTransform->c #} p)
                     <*> (realToFrac <$> {# get cpTransform->d #} p)
                     <*> (realToFrac <$> {# get cpTransform->tx #} p)
                     <*> (realToFrac <$> {# get cpTransform->ty #} p)

-- | Pointer to 'Transform'
{# pointer *cpTransform as TransformPtr -> Transform #}

-- | Collision type
type CollisionType = WordPtr

type CPBool = {# type cpBool #}
