module Main where

------------------------------------------------------------------------

import Boids
import Linear.Vector
import Linear.V3
import Linear.V2
import Graphics.UI.GLUT hiding (position, Radius)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Control.Concurrent (threadDelay)
import Control.Lens -- sorry

------------------------------------------------------------------------

-- |A global world state is just a list of 'Boids'.
type World  = [Boid]

-- |Action update for a 'Boid'. The 'update' function should map this across the boids
type Action = World -> Boid -> Boid

-- |Update the entire world state by mapping an 'Action' across each 'Boid'
update :: Action -> World -> World
update a w = map (a w) w

inSphere :: V3 Float -> Radius -> V3 Float -> Bool
      -- :: V3 Float -> Float -> V3 Float -> Bool
inSphere p_0 r p_i = ((x_i - x)^n + (y_i - y)^n + (z_i - z)^n) <= r^n
    where x_i = p_i ^._x
          y_i = p_i ^._y
          z_i = p_i ^._z
          x   = p_0 ^._x
          y   = p_0 ^._y
          z   = p_0 ^._z
          n   = 2 :: Integer

inCircle :: Point -> Radius -> Point -> Bool
      -- :: V2 Float -> Radius -> V2 Float -> Bool
inCircle p_0 r p_i = ((x_i - x)^n + (y_i - y)^n) <= r^n
  where x_i = p_i ^._x
        y_i = p_i ^._y
        x   = p_0 ^._x
        y   = p_0 ^._y
        n   = 2 :: Integer

-- |Find the neighborhood for a given 'Boid'
neighborhood :: World -> Boid -> Perception
          -- :: [Boid] -> Boid -> [Boid]
neighborhood world self = filter (inCircle cent rad . position) world
    where cent = position self
          rad  = radius self

emptyStep :: Action
emptyStep w b = emptyBehaviour (neighborhood w b) b

eqWeightStep :: Action
eqWeightStep w b = equalWeightsBehaviour (neighborhood w b) b

cohesiveStep :: Action
cohesiveStep w b = cohesiveBehaviour (neighborhood w b) b

initWorld :: Int -> World
initWorld n = replicate n $ Boid origin vel 500.0
  where origin = V2 0 0
        vel    = V2 1 1

toGLVertex :: V3 Float -> Vertex3 GLfloat
toGLVertex (V3 x y z) = Vertex3 (glfloat x) (glfloat y) (glfloat z)
  where glfloat a = realToFrac a :: GLfloat

animate :: IORef World -> IdleCallback
animate r = do
  threadDelay 1000000
  modifyIORef r (update cohesiveStep)
  postRedisplay Nothing

display :: IORef World -> DisplayCallback
display r' = do
  w <- readIORef r'
  clear [ColorBuffer]
  -- putStrLn "render"
  mapM_ print w
  renderPrimitive Points $ mapM_ (vertex . toGLVertex . position) w
  flush

main :: IO ()
main = do
  initialWindowSize $= Size 500 500
  _ <- getArgsAndInitialize
  _ <- createWindow "Hello World"
  r <- newIORef $ initWorld 10
  displayCallback $= display r
  idleCallback $= Just (animate r)
  mainLoop
