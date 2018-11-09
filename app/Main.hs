{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Chiphunk.Low
import Data.Functor
import Text.Printf (printf)
import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async

import qualified Graphics.NanoVG.Simple as NS
import qualified NanoVG                 as NVG
import           Data.IORef

main :: IO ()
main = do
  dm <- newEmptyMVar
  race_
    (simulate dm)
    (display dm)

simulate :: MVar [VisObj] -> IO ()
simulate dm = do
  let gravity = Vect 0 (-100)
  -- Create an empty space.
  space <- spaceNew
  spaceSetGravity space gravity

  static <- spaceGetStaticBody space

  -- Add a static line segment shape for the ground.
  -- We'll make it slightly tilted so the ball will roll off.
  -- We attach it to a static body to tell Chipmunk it shouldn't be movable.
  let (segA, segB) = (Vect (-20) (-5), Vect 20 (-25))
  ground <- segmentShapeNew static segA segB 0
  shapeSetElasticity ground 0.6
  shapeSetFriction ground 1

  spaceAddShape space ground

  -- Now let's make a ball that falls onto the line and rolls off.
  -- First we need to make a cpBody to hold the physical properties of the object.
  -- These include the mass, position, velocity, angle, etc. of the object.
  -- Then we attach collision shapes to the cpBody to give it a size and shape.

  let radius = 5
  let mass = 1
  let mass100 = 100

  -- The moment of inertia is like mass for rotation
  -- Use the cpMomentFor*() functions to help you approximate it.
  let moment = momentForCircle mass 0 radius (Vect 0 0)
  let moment100 = momentForCircle mass100 0 radius (Vect 0 0)

  -- The cpSpaceAdd*() functions return the thing that you are adding.
  -- It's convenient to create and add an object in one line.
  ballBody <- bodyNew mass moment
  spaceAddBody space ballBody

  -- Now we create the collision shape for the ball.
  -- You can create multiple collision shapes that point to the same body.
  -- They will all be attached to the body and move around to follow it.
  ballShape <- circleShapeNew ballBody radius (Vect 0 0)
  shapeSetFriction ballShape 0.9
  shapeSetElasticity ballShape 1
  spaceAddShape space ballShape

  anotherBall <- bodyNew mass100 moment100
  spaceAddBody space anotherBall

  anotherBallShape <- circleShapeNew anotherBall radius (Vect 0 0)
  shapeSetFriction anotherBallShape 0.9
  shapeSetElasticity anotherBallShape 0.4
  spaceAddShape space anotherBallShape

  putMVar dm
    [ mkStaticObj $ Segment segA segB
    , mkBallBody ballBody radius
    , mkBallBody anotherBall radius
    ]

  void $ forever $ do
    bodySetPosition ballBody (Vect (-15) 30)
    bodySetPosition anotherBall (Vect (-5) 75)
    -- need to reset ball velocity after previous iteration
    bodySetVelocity ballBody (Vect 0 0)
    bodySetAngularVelocity ballBody 0
    bodySetVelocity anotherBall (Vect 0 0)
    bodySetAngularVelocity anotherBall 0

    -- Now that it's all set up, we simulate all the objects in the space by
    -- stepping forward through time in small increments called steps.
    -- It is *highly* recommended to use a fixed size time step.
    let timeStep = 1/60
    runFor 3 timeStep $ \time -> do
      pos <- bodyGetPosition ballBody
      vel <- bodyGetVelocity ballBody
      printf "Time is %4.2f. ballBody is at (%6.2f, %6.2f), it's velocity is (%6.2f, %6.2f).\n"
             time (vX pos) (vY pos) (vX vel) (vY vel)

      threadDelay $ round $ timeStep * 1000 * 1000
      spaceStep space timeStep

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
  NS.run 800 600 "Chiphunk" $
    NS.Window
      { NS.winInit = \_ -> pure ()
      , NS.winRender = \_ ctx -> forM_ d $ render ctx
      , NS.winAfterRender = \_ _ -> pure ()
      }
  where
    render ctx (VisObj ioS) = do
      NVG.beginPath ctx
      NVG.strokeColor ctx (NVG.Color 1 1 1 1)
      ioS >>= \case
        Segment (Vect ax ay) (Vect bx by) -> do
          putStrLn $ show ax <> " x " <> show ay <> " -> " <> show bx <> "x" <> show by
          NVG.moveTo ctx (400 + t ax) (300 - t ay)
          NVG.lineTo ctx (400 + t bx) (300 - t by)
          NVG.stroke ctx
        Ball (Vect x y) r _ -> do
          putStrLn $ show x <> " x " <> show y <> " R " <> show r
          NVG.circle ctx (400 + t x) (300 - t y) (t r)
          NVG.stroke ctx
        -- Ball (Vect x y) r a -> pure $
          -- G.Translate (t x) (t y) $
          -- G.Rotate (realToFrac (-a)) $
          -- G.Pictures
            -- [ G.Circle (t r)
            -- , G.Line [(0, 0), (t r / 2, 0)]
            -- ]
    t :: RealFrac frac => Double -> frac
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

mkRefObj :: IORef VisShape -> VisObj
mkRefObj r = VisObj $ readIORef r

mkStaticObj :: VisShape -> VisObj
mkStaticObj = VisObj . pure

mkBallBody :: Body -> Double -> VisObj
mkBallBody b r = VisObj $ Ball <$> bodyGetPosition b <*> pure r <*> bodyGetAngle b
