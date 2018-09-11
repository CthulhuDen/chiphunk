{-# LANGUAGE LambdaCase #-}
module Main where

import Chiphunk.Low
import Text.Printf
import Data.Functor
import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async

import qualified Gloss.Data as G
import qualified Graphics.Gloss.Interface.IO.Game as G hiding (SpecialKey, playIO)

import           Foreign.Storable
import           Foreign.Ptr
import           Foreign.Marshal.Array
import           Control.Exception.Safe
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GL
import           Graphics.Rendering.OpenGL.GL (($=), get)
import qualified Graphics.UI.GLFW as GLFW

data Coords = Coords
  { coordX :: !Float
  , coordY :: !Float
  }

instance Storable Coords where
  sizeOf _    = 2 * sizeOf (undefined :: Float)
  alignment _ = alignment (undefined :: Float)
  poke p (Coords x y) = poke (castPtr p) x *> pokeElemOff (castPtr p) 1 y
  peek p = Coords <$> peek (castPtr p) <*> peekElemOff (castPtr p) 1

data Vertex = Vertex
  { vertCoords :: !Coords
  }

instance Storable Vertex where
  sizeOf _ = sizeOf (undefined :: Coords)
  alignment _ = alignment (undefined :: Coords)
  poke p (Vertex c) = poke (castPtr p) c
  peek p = Vertex <$> peek (castPtr p)

main :: IO ()
main = do
  vertPtr <- newArray vertices

  win <- initGLFW
  prog <- initGL (length vertices) vertPtr
  loop win prog
  finalize win
  where
    vertices =
      [ Vertex $ Coords (-0.6) (-0.6)
      , Vertex $ Coords 0 0.6
      , Vertex $ Coords 0.6 (-0.6)
      ]

    vertexShaderSource = GL.packUtf8 $ unlines
      [ "#version 150 core"
      , ""
      , "in vec2 position;"
      , ""
      , "uniform mat4 mvp;"
      , ""
      , "void main()"
      , "{"
      , "  gl_Position = mvp * vec4(position, 0.0, 1.0);"
      , "}"
      ]

    fragmentShaderSource = GL.packUtf8 $ unlines
      [ "#version 150 core"
      , ""
      , "out vec4 color;"
      , ""
      , "void main()"
      , "{"
      , "  color = vec4(1.0, 1.0, 1.0, 1.0);"
      , "}"
      ]

    initGLFW = do
      GLFW.setErrorCallback $ Just $ \err msg ->
        putStrLn $ "Error " <> show err <> ": " <> msg
      do res <- GLFW.init
         when (not res) $ throwString "Could not initialize GLFW"
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 2
      win <- GLFW.createWindow 500 500 "glfw" Nothing Nothing >>= \case
        Just win -> pure win
        Nothing  -> do
          GLFW.terminate
          throwString "Could not create window"
      GLFW.makeContextCurrent $ Just win
      pure win

    initGL vertCount vertPtr = do
      vao <- GL.genObjectName
      GL.bindVertexArrayObject $= Just vao

      vertex_buffer <- GL.genObjectName
      GL.bindBuffer GL.ArrayBuffer $= Just vertex_buffer
      GL.bufferData GL.ArrayBuffer $= (fromIntegral $ vertCount * sizeOf (undefined :: Vertex), vertPtr, GL.StaticDraw)

      vert <- makeShader GL.VertexShader vertexShaderSource
      frag <- makeShader GL.FragmentShader fragmentShaderSource

      prog <- GL.createProgram
      GL.attachShader prog vert
      GL.attachShader prog frag
      GL.bindFragDataLocation prog "color" $= 0
      GL.linkProgram prog

      pos <- GL.get $ GL.attribLocation prog "position"
      GL.vertexAttribArray pos $= GL.Enabled
      GL.vertexAttribPointer pos $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 nullPtr)

      GL.currentProgram $= Just prog

      pure prog
      where
        makeShader typ source = do
          shader <- GL.createShader typ
          GL.shaderSourceBS shader $= source
          GL.compileShader shader

          res <- get $ GL.compileStatus shader
          compLog <- get $ GL.shaderInfoLog shader

          unless res $ throwString $ "Shader compilation FAILED: \n" <> compLog
          unless (null compLog) $ throwString $ "Shader compiled with warnings: \n===\n" <> compLog <> "\n===\n"

          pure shader

    loop win prog = do
      uniMvp <- GL.get $ GL.uniformLocation prog "mvp"
      go uniMvp
      where
        go uniMvp = do
          shouldClose <- GLFW.windowShouldClose win
          unless shouldClose $ do
            checkGLError

            (width, height) <- GLFW.getFramebufferSize win
            GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

            GL.clear [GL.ColorBuffer]

            m <- get $ GL.matrix $ Just $ GL.Modelview 0 :: IO (GL.GLmatrix GL.GLfloat)
            GL.loadIdentity

            GL.uniformv uniMvp $= m

            GL.drawArrays GL.Triangles 0 3

            GLFW.swapBuffers win

            GLFW.pollEvents

            go uniMvp
        checkGLError = get GL.errors >>= \case
          []  -> pure ()
          ers -> throwString $ "OpenGL reported errors: " <> show ers

    finalize win = do
      GLFW.destroyWindow win
      GLFW.terminate

  -- dm <- newEmptyMVar
  -- race_ (simulate dm) (display dm)

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

  putMVar dm [mkStaticObj $ Segment segA segB, mkBallBody ballBody radius, mkBallBody anotherBall radius]

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
