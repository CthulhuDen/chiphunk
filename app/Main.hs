module Main where

import Chiphunk.Low
import Control.Monad
import Text.Printf

main :: IO ()
main = do
  let gravity = Vect 0 (-100)

  -- Create an empty space.
  space <- spaceNew
  spaceSetGravity space gravity

  -- Add a static line segment shape for the ground.
  -- We'll make it slightly tilted so the ball will roll off.
  -- We attach it to a static body to tell Chipmunk it shouldn't be movable.
  static <- spaceGetStaticBody space
  ground <- segmentShapeNew static (Vect (-20) 5) (Vect 20 (-5)) 0
  shapeSetFriction ground 1
  spaceAddShape space ground

  -- Now let's make a ball that falls onto the line and rolls off.
  -- First we need to make a cpBody to hold the physical properties of the object.
  -- These include the mass, position, velocity, angle, etc. of the object.
  -- Then we attach collision shapes to the cpBody to give it a size and shape.

  let radius = 5
  let mass = 1

  -- The moment of inertia is like mass for rotation
  -- Use the cpMomentFor*() functions to help you approximate it.
  let moment = momentForCircle mass 0 radius (Vect 0 0)

  -- The cpSpaceAdd*() functions return the thing that you are adding.
  -- It's convenient to create and add an object in one line.
  ballBody <- bodyNew mass moment
  spaceAddBody space ballBody
  bodySetPosition ballBody (Vect 0 15)

  -- Now we create the collision shape for the ball.
  -- You can create multiple collision shapes that point to the same body.
  -- They will all be attached to the body and move around to follow it.
  ballShape <- circleShapeNew ballBody radius (Vect 0 0)
  spaceAddShape space ballShape
  shapeSetFriction ballShape 0.7

  -- Now that it's all set up, we simulate all the objects in the space by
  -- stepping forward through time in small increments called steps.
  -- It is *highly* recommended to use a fixed size time step.
  let timeStep = 1/60
  runFor 2 timeStep $ \t -> do
    pos <- bodyGetPosition ballBody
    vel <- bodyGetVelocity ballBody
    printf "Time is %4.2f. ballBody is at (%6.2f, %6.2f), it's velocity is (%6.2f, %6.2f).\n"
           t (vX pos) (vY pos) (vX vel) (vY vel)

    spaceStep space timeStep

  shapeFree ballShape
  bodyFree ballBody
  shapeFree ground
  spaceFree space
  where
    runFor t s inner = go t
      where
        go t'
          | t' <= 0   = pure ()
          | otherwise = inner (t - t') *> go (t' - s)
