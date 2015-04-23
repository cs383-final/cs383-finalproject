module Main where

------------------------------------------------------------------------

import Boids
import Linear.Vector
import Linear.V3
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

inSphere :: Point -> Radius -> Point -> Bool
      -- :: V3 Float -> Float -> V3 Float -> Bool
inSphere p_0 r p_i = ((x_i - x)^2 + (y_i - y)^2 + (z_i - z)^2) <= r^2
    where x_i = p_i ^._x
          y_i = p_i ^._y
          z_i = p_i ^._z
          x   = p_0 ^._x
          y   = p_0 ^._y
          z   = p_0 ^._z


-- |Find the neighborhood for a given 'Boid'
neighborhood :: World -> Boid -> Perception
          -- :: [Boid] -> Boid -> [Boid]
neighborhood world self = filter (\x -> inSphere centre rad $ position x) world
    where centre = position self
          rad    = radius self

emptyStep :: Action
emptyStep w b = emptyBehaviour (neighborhood w b) b

eqWeightStep :: Action
eqWeightStep w b = equalWeightsBehaviour (neighborhood w b) b

initWorld :: Int -> World
initWorld n = replicate n $ Boid origin zero 10.0
  where origin = V3 0 0 0

toGLVertex :: V3 Float -> Vertex3 GLfloat
toGLVertex (V3 x y z) = Vertex3 (glfloat x) (glfloat y) (glfloat z)
  where glfloat a = realToFrac a :: GLfloat

animate :: IORef World -> IdleCallback
animate r = do
  threadDelay 10000
  modifyIORef r (update eqWeightStep)
  postRedisplay Nothing

display :: IORef World -> DisplayCallback
display r' = do
  w <- readIORef r'
  clear [ColorBuffer]
  putStrLn "render"
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
