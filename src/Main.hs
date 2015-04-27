module Main where

------------------------------------------------------------------------

import Boids
import Linear.Vector
import Linear.V3
import Linear.V2
import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.Pure.Simulate hiding (Point)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Control.Concurrent (threadDelay)
import Control.Lens -- sorry
import System.Random

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

swarmStep :: Action
swarmStep w b = swarmBehaviour (neighborhood w b) b

randomBoid :: RandomGen g => g -> (Boid, g)
randomBoid g = case randomR (-50,50) g of
                    (r', g') -> ((Boid (pos r') origin rad), g')
  where pos x  = V2 x x
        origin = V2 0 0
        rad    = 500.0


initWorld :: RandomGen g => g -> Int -> World
initWorld g n = map fst $ replicate n $ randomBoid g

norm :: Float -> Float -> Float
norm pos dim = (pos / dim) * 2 - (dim / 2)

drawBoid :: Boid -> Picture
drawBoid (Boid (V2 x y) _ _) = Translate x y (Circle 5)

drawWorld :: World -> Picture
drawWorld = Pictures . map drawBoid

advanceWorld :: ViewPort -> Float -> World -> World
advanceWorld _ _ = update swarmStep

main :: IO ()
main = do

  g <- getStdGen

  simulate (InWindow "Boids" (300, 300) (10, 10))
    white   -- background color
    20       -- updates per second
    (initWorld g 10)
    drawWorld
    advanceWorld
