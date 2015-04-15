module Main where
------------------------------------------------------------------------

import Boids
------------------------------------------------------------------------

-- A global world state is just a list of boids.
type World  = [Boid]

-- Update the entire world state by mapping an Action across each Boid
type Update = World -> World

-- Action update for a Boid. The Update function should map this across the boids
type Action = World -> (Boid -> Boid) 

main :: IO ()
main = putStrLn "This program needs to be written :("
