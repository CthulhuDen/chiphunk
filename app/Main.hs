module Main where

import Chiphunk
import Control.Monad
import Text.Printf

main :: IO ()
main = do
  let gravity = cpv 0 (-100)

  -- Create an empty space.
  space <- cpSpaceNew
  cpSpaceSetGravity space gravity

  -- Add a static line segment shape for the ground.
  -- We'll make it slightly tilted so the ball will roll off.
  -- We attach it to a static body to tell Chipmunk it shouldn't be movable.
  static <- cpSpaceGetStaticBody space
  ground <- cpSegmentShapeNew static (cpv (-20) 5) (cpv 20 (-5)) 0
  cpShapeSetFriction ground 1
  cpSpaceAddShape space ground

  -- Now let's make a ball that falls onto the line and rolls off.
  -- First we need to make a cpBody to hold the physical properties of the object.
  -- These include the mass, position, velocity, angle, etc. of the object.
  -- Then we attach collision shapes to the cpBody to give it a size and shape.

  let radius = 5
  let mass = 1

  -- The moment of inertia is like mass for rotation
  -- Use the cpMomentFor*() functions to help you approximate it.
  let moment = cpMomentForCircle mass 0 radius (cpv 0 0)

  -- The cpSpaceAdd*() functions return the thing that you are adding.
  -- It's convenient to create and add an object in one line.
  ballBody <- cpBodyNew mass moment
  cpSpaceAddBody space ballBody
  cpBodySetPosition ballBody (cpv 0 15)

  -- Now we create the collision shape for the ball.
  -- You can create multiple collision shapes that point to the same body.
  -- They will all be attached to the body and move around to follow it.
  ballShape <- cpCircleShapeNew ballBody radius (cpv 0 0)
  cpSpaceAddShape space ballShape
  cpShapeSetFriction ballShape 0.7

  -- Now that it's all set up, we simulate all the objects in the space by
  -- stepping forward through time in small increments called steps.
  -- It is *highly* recommended to use a fixed size time step.
  let timeStep = 1/60
  runFor 2 timeStep $ \t -> do
    pos <- cpBodyGetPosition ballBody
    vel <- cpBodyGetVelocity ballBody
    printf "Time is %4.2f. ballBody is at (%6.2f, %6.2f), it's velocity is (%6.2f, %6.2f).\n"
           t (cpvX pos) (cpvY pos) (cpvX vel) (cpvY vel)

    cpSpaceStep space timeStep

  cpShapeFree ballShape
  cpBodyFree ballBody
  cpShapeFree ground
  cpSpaceFree space
  where
    runFor t s inner = go t
      where
        go t'
          | t' <= 0   = pure ()
          | otherwise = inner (t - t') *> go (t' - s)
