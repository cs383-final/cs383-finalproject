module Boids where

------------------------------------------------------------------------------

import Linear.V3

------------------------------------------------------------------------------

type Vector = V3 Float
type Point  = V3 Float
type Radius = Float

data Boid = Boid { position :: !Point
                 , target   :: !Point
                 , velocity :: !Vector
                 , radius   :: !Radius
                 }
  deriving (Show)

type Update = Boid -> Boid
type Perception = [Boid]
type Behaviour = Perception -> Update

emptyBehaviour :: Behaviour
            -- :: [Boid] -> Boid -> Boid
emptyBehaviour _ b = b

positions :: [Boid] -> [Vector]
positions = map position


-- |Find the centre of a list of boids
centre :: [Boid] -> Vector
centre boids =
    let m = length boids in
    sumV . map(^/ m) boids

separation :: Boid -> [Boid] -> Vector
separation = undefined

-- |Find the cohesion vector for a boid given a neighborhood.
-- |
-- |Cohesion is calculated in two steps. First, the centre /c/i is calculated
-- |for the visible neighborhood. Then, the cohesion vector /k/i is calculated
-- |by subtracting the current position /p/i from /c/i
cohesion :: Boid -> [Boid] -> Vector
cohesion self neighbors = (centre neighbors) - self position

alignment :: Boid -> [Boid] -> Vector
alignment = undefined
