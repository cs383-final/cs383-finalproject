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

emptyStep :: Action
emptyStep w b = emptyBehaviour (neighborhood w b) b

-- |Find the neighborhood for a given 'Boid'
neighborhood :: World -> Boid -> Perception
neighborhood world self = filter inSphere world
    where inSphere boid = ((xi - x)^2 + (yi - y)^2 + (zi - z)^2) <= r
        where p  = position boid
              r  = (radius self)^2
              xi = p ^._x
              yi = p ^._y
              zi = p ^._z

initWorld :: World
initWorld = replicate 3 $ Boid origin zero 10.0
  where origin = V3 0 0 0

main :: IO ()
main = mapM_ print $ take 3 $ iterate (update emptyStep) initWorld
