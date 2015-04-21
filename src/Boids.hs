module Boids where

------------------------------------------------------------------------------

import Linear.V3
import Linear.Vector

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
    let m = fromIntegral $ length boids :: Float
    in sumV $ map (^/ m) $ positions boids

-- |Find the separation steer vector for a boid given a neighborhood.
-- |
-- |The separation steer vector /s/i can be computed simply by summing the
-- |boid's position /p/i against each other position /p/j and taking the
-- |negative sum of these vectors.
separation :: Boid -> [Boid] -> Vector
separation self neighbors =
    let p = position self
    in sumV . map (^-^ p) $ positions neighbors

-- |Find the cohesion steer vector for a boid given a neighborhood.
-- |
-- |Cohesion is calculated in two steps. First, the centre /c/i is calculated
-- |for the visible neighborhood. Then, the cohesion vector /k/i is calculated
-- |by subtracting the current position /p/i from /c/i
cohesion :: Boid -> [Boid] -> Vector
cohesion self neighbors =
    let p = position self
    in (centre neighbors) - p

-- |Find the alignment steer vector for a boid given a neighborhood.
alignment :: Boid -> [Boid] -> Vector
alignment _ [] = V3 0 0 0
alignment self =
    let m = fromIntegral . length  :: Float
    in ^/ m . sumV . map (velocity)
