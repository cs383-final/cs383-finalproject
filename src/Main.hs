module Main where
------------------------------------------------------------------------

import Boids
------------------------------------------------------------------------

-- A global world state is just a list of boids.
type World  = [Boid]

-- Action update for a Boid. The Update function should map this across the boids
type Action = World -> (Boid -> Boid) 

-- Update the entire world state by mapping an Action across each Boid
update :: Action -> World -> World
update a w = map a w

main :: IO ()
main = putStrLn "This program needs to be written :("
