{-# LANGUAGE LambdaCase #-}

module Main where

import Chiphunk.Low
import Text.Printf
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async

import qualified Gloss.Data as G
import qualified Graphics.Gloss.Interface.IO.Game as G hiding (SpecialKey, playIO)

main :: IO ()
main = do
  dm <- newEmptyMVar
  race_ (simulate dm) (display dm)

simulate :: MVar [VisObj] -> IO ()
simulate dm = do
  let gravity = Vect 0 (-100)

  -- Create an empty space.
  space <- spaceNew
  spaceSetGravity space gravity

  -- Add a static line segment shape for the ground.
  -- We'll make it slightly tilted so the ball will roll off.
  -- We attach it to a static body to tell Chipmunk it shouldn't be movable.
  static <- spaceGetStaticBody space
  let (segA, segB) = (Vect (-20) 5, Vect 20 (-5))
  ground <- segmentShapeNew static segA segB 0
  shapeSetElasticity ground 0.7
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
  bodySetPosition ballBody (Vect (-15) 15)

  putMVar dm [mkStaticObj $ Segment segA segB, mkBallBody ballBody radius]

  -- Now we create the collision shape for the ball.
  -- You can create multiple collision shapes that point to the same body.
  -- They will all be attached to the body and move around to follow it.
  ballShape <- circleShapeNew ballBody radius (Vect 0 0)
  spaceAddShape space ballShape
  shapeSetFriction ballShape 0.9
  shapeSetElasticity ballShape 0.7

  -- Now that it's all set up, we simulate all the objects in the space by
  -- stepping forward through time in small increments called steps.
  -- It is *highly* recommended to use a fixed size time step.
  let timeStep = 1/60
  runFor 2 timeStep $ \time -> do
    pos <- bodyGetPosition ballBody
    vel <- bodyGetVelocity ballBody
    printf "Time is %4.2f. ballBody is at (%6.2f, %6.2f), it's velocity is (%6.2f, %6.2f).\n"
           time (vX pos) (vY pos) (vX vel) (vY vel)

    spaceStep space timeStep
    threadDelay $ round $ 1000000 * timeStep

  shapeFree ballShape
  bodyFree ballBody
  shapeFree ground
  spaceFree space
  where
    runFor time step inner = go time
      where
        go time'
          | time' <= 0 = pure ()
          | otherwise  = inner (time - time') *> go (time' - step)

display :: MVar [VisObj] -> IO ()
display dm = do
  d <- takeMVar dm
  quit <- newEmptyMVar
  race_ (takeMVar quit) $
   G.playIO (G.InWindow "chiphunk" (500, 500) (0, 0))
           (G.makeColor 0.5 0.5 0.5 1)
           60 ()
           (\() -> G.Pictures <$> mapM render d)
           (\e _ () -> case e of
              -- Still not working, because playIO somehow ignores exception thrown from 'race_'!
              G.KeyPress (G.SpecialKey G.KeyEsc) G.Down -> putMVar quit ()
              _                                         -> pure ())
           (\_ _ () -> pure ())
  where
    render (VisObj ioS) = ioS >>= \case
      Segment (Vect ax ay) (Vect bx by) -> pure $
        G.Line [(t ax, t ay), (t bx, t by)]
      Ball (Vect x y) r a -> pure $ G.Translate (t x) (t y) $ G.Rotate (t (-a)) $
        G.Pictures
        [ G.Circle (t r)
        , G.Line [(0, 0), (t r / 2, 0)]
        ]
    t :: Double -> Float
    t = realToFrac . (*10)

data VisShape =
    Segment
    { segEndpointA :: Vect
    , segEndpointB :: Vect
    }
  | Ball
    { ballCenter :: Vect
    , ballRadius :: Double
    , ballAngle :: Double
    }
  deriving Show

newtype VisObj = VisObj (IO VisShape)

mkStaticObj :: VisShape -> VisObj
mkStaticObj = VisObj . pure

mkBallBody :: Body -> Double -> VisObj
mkBallBody b r = VisObj $ Ball <$> bodyGetPosition b <*> pure r <*> bodyGetAngle b
