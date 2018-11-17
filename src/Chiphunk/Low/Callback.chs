-- | Description: Collision handler definition
-- Module provides definitions for collision handlers.
module Chiphunk.Low.Callback
  ( CollisionCallback
  , CollisionHandler (..)
  , CollisionHandlerPtr
  , spaceAddCollisionHandler
  , spaceAddWildcardHandler
  , spaceAddDefaultCollisionHandler
  , modifyCollisionHandler
  , mkCallback
  , mkCallbackB
  , PostStepFunc
  , spaceAddPostStepCallback
  ) where

import Control.Applicative (liftA)
import Control.Exception.Safe
import Data.Bool (bool)
import Foreign

{# import Chiphunk.Low.Types #}

#include <chipmunk/chipmunk.h>

-- | Collision callback
type CollisionCallback ret = Arbiter -> Space -> DataPtr -> IO ret

-- | This collision handler processes collisions between objects of type @typeA@ and @typeB@.
-- Fill the desired collision callback functions- they are documented above. A user definable context pointer
-- @userData@ is included for your convenience. This pointer is provided as an argument in each callback function.
--
-- A collision handler is a set of 4 function callbacks for the different collision events that Chipmunk recognizes.
data CollisionHandler = CollisionHandler
  { chTypeA         :: !CollisionType                   -- ^ typeA
  , chTypeB         :: !CollisionType                   -- ^ typeB
  , chBeginFunc     :: !(FunPtr (CollisionCallback CPBool))
    -- ^ Two shapes just started touching for the first time this step. Return true from the callback
    -- to process the collision normally or false to cause Chipmunk to ignore the collision entirely.
    -- If you return false, the preSolve and postSolve callbacks will never be run, but you will still recieve
    -- a separate event when the shapes stop overlapping.
  , chPreSolveFunc  :: !(FunPtr (CollisionCallback CPBool))
    -- ^ Two shapes are touching during this step. Return false from the callback to make Chipmunk ignore the collision
    -- this step or true to process it normally. Additionally, you may override collision values using
    -- 'Chiphunk.Low.Arbiter.arbiterFriction', 'Chiphunk.Low.Arbiter.arbiterRestitution' or
    -- 'Chiphunk.Low.arbiterSurfaceVelocity' to provide custom friction, elasticity, or surface velocity values.
    -- See 'Arbiter' for more info.
  , chPostSolveFunc :: !(FunPtr (CollisionCallback ()))
    -- ^ Two shapes are touching and their collision response has been processed. You can retrieve the collision
    -- impulse or kinetic energy at this time if you want to use it to calculate sound volumes or damage amounts.
    -- See 'Arbiter' for more info.
  , chSeparateFunc  :: !(FunPtr (CollisionCallback ()))
    -- ^ Two shapes have just stopped touching for the first time this step. To ensure that begin/separate
    -- are always called in balanced pairs, it will also be called when removing a shape while its in contact
    -- with something or when deallocating the space.
  , cpUserData      :: !DataPtr                         -- ^ userData
  } deriving Show

instance Storable CollisionHandler where
  sizeOf _ = {# sizeof cpCollisionHandler #}
  alignment _ = {# alignof cpCollisionHandler #}
  poke p (CollisionHandler typA typB beginFunc preSolveFunc postSolveFunc separateFunc userData) = do
    {# set cpCollisionHandler->typeA #} p $ fromIntegral typA
    {# set cpCollisionHandler->typeB #} p $ fromIntegral typB
    {# set cpCollisionHandler->beginFunc #} p beginFunc
    {# set cpCollisionHandler->preSolveFunc #} p preSolveFunc
    {# set cpCollisionHandler->postSolveFunc #} p postSolveFunc
    {# set cpCollisionHandler->separateFunc #} p separateFunc
    {# set cpCollisionHandler->userData #} p userData
  peek p = CollisionHandler <$> (fromIntegral <$> {# get cpCollisionHandler->typeA #} p)
                            <*> (fromIntegral <$> {# get cpCollisionHandler->typeB #} p)
                            <*> {# get cpCollisionHandler->beginFunc #} p
                            <*> {# get cpCollisionHandler->preSolveFunc #} p
                            <*> {# get cpCollisionHandler->postSolveFunc #} p
                            <*> {# get cpCollisionHandler->separateFunc #} p
                            <*> {# get cpCollisionHandler->userData #} p

foreign import ccall unsafe "wrapper"
  mkCallback' :: CollisionCallback () -> IO (FunPtr (CollisionCallback ()))

-- | Make callback. Need to free afterwards.
mkCallback :: CollisionCallback () -> IO (FunPtr (CollisionCallback ()))
mkCallback = mkCallback'

foreign import ccall unsafe "wrapper"
  mkCallbackB' :: CollisionCallback CPBool -> IO (FunPtr (CollisionCallback CPBool))

-- | Make callback. Need to free afterwards.
mkCallbackB :: CollisionCallback Bool -> IO (FunPtr (CollisionCallback CPBool))
mkCallbackB = mkCallbackB' . liftA (liftA $ liftA $ liftA $ bool 0 1)

-- | Pointer to collision handler
{# pointer *cpCollisionHandler as CollisionHandlerPtr -> CollisionHandler #}

-- | Add a 'CollisionHandler' for specific collision type pair or return the existing handler for the type pair.
-- Whenever shapes with collision types (cpShape.collision_type) a and b collide,
-- this handler will be used to process the collision events. When a new collision handler is created,
-- the callbacks will all be set to builtin callbacks that perform the default behavior
-- (call the wildcard handlers, and accept all collisions).
{# fun unsafe cpSpaceAddCollisionHandler as spaceAddCollisionHandler
  { `Space'
  , fromIntegral `CollisionType' -- ^ a
  , fromIntegral `CollisionType' -- ^ b
  } -> `CollisionHandlerPtr' #}

-- | Add a wildcard collision handler for given collision type. This handler will be used any time an object
-- with this type collides with another object, regardless of its type. A good example is a projectile
-- that should be destroyed the first time it hits anything. There may be a specific collision handler
-- and two wildcard handlers. It’s up to the specific handler to decide if and when to call the wildcard handlers
-- and what to do with their return values. (See arbiterCallWildcard* below)
-- When a new wildcard handler is created, the callbacks will all be set to builtin callbacks
-- that perform the default behavior. (accept all collisions in begin and preSolve, or do nothing for postSolve
-- and separate.
{# fun unsafe cpSpaceAddWildcardHandler as spaceAddWildcardHandler
  { `Space'
  , fromIntegral `CollisionType' -- ^ type
  } -> `CollisionHandlerPtr' #}

-- | Return a reference to the default collision handler or that is used to process all collisions
-- that don’t have a more specific handler. The default behavior for each of the callbacks
-- is to call the wildcard handlers, ANDing their return values together if applicable.
{# fun unsafe cpSpaceAddDefaultCollisionHandler as spaceAddDefaultCollisionHandler
  { `Space'
  } -> `CollisionHandlerPtr' #}

-- | Use this helper function to modify collision handler.
--
-- @
-- spaceAddCollisionHandler s t1 t2 >>= flip modifyColliionHandler (\ch -> pure ch {chSeparateFunc = separateCollback})
-- @
modifyCollisionHandler :: CollisionHandlerPtr -> (CollisionHandler -> IO CollisionHandler) -> IO ()
modifyCollisionHandler chPtr inner = peek chPtr >>= inner >>= poke chPtr

-- | Function type used for postStep callbacks. @space@ is the space the callback was registered on,
-- @obj@ is the pointer value you supplied as the key, and @data@ is a user definable pointer you can use
-- to pass in as a context value.
type PostStepFunc
  = Space   -- ^ space
  -> Ptr () -- ^ obj
  -> Ptr () -- ^ data
  -> IO ()

foreign import ccall "wrapper"
  mkPostStep :: PostStepFunc -> IO (FunPtr PostStepFunc)

-- | Add @func@ to be called before 'spaceStep' returns. @key@ and @data@ will be passed to your function.
-- Only the first callback registered for any unique value of @key@ will be recorded.
--
-- It returns 'True' if the callback is scheduled and 'False' when the key has already been used.
--
-- __The behavior of adding a postStep callback from outside of a collision handler or query callback is undefined.__
{# fun unsafe cpSpaceAddPostStepCallback as spaceAddPostStepCallback
  { `Space'            -- ^ space
  , mk* `PostStepFunc' -- ^ func
  , `Ptr ()'           -- ^ key
  , `Ptr ()'           -- ^ data
  } -> `Bool' #}
  where
    mk f = mkPostStep f `bracket` freeHaskellFunPtr
