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
emptyStep w b = emptyStrategy (neighborhood w b) b

neighborhood :: World -> Boid -> Perception
neighborhood _ _ = []  -- TODO

initWorld :: World
initWorld = replicate 3 $ Boid origin origin zero 10.0
  where origin = V3 0 0 0

main :: IO ()
main = mapM_ print $ take 3 $ iterate (update emptystep) initWorld
