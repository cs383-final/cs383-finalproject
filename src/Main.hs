module Main where

------------------------------------------------------------------------

import Boids
import Linear.Vector
import Linear.V3

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
neighborhood world self = filter (inSphere centre rad) world
    where centre = position self
          rad    = radius self

emptyStep :: Action
emptyStep w b = emptyBehaviour (neighborhood w b) b

initWorld :: World
initWorld = replicate 3 $ Boid origin zero 10.0
  where origin = V3 0 0 0

main :: IO ()
main = mapM_ print $ take 3 $ iterate (update emptyStep) initWorld
