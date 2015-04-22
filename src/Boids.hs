module Boids where

------------------------------------------------------------------------------

import Linear.V3
import Linear.Vector

------------------------------------------------------------------------------

type Vector = V3 Float
type Point  = V3 Float
type Radius = Float

data Boid = Boid { position :: !Point
                 , velocity :: !Vector
                 , radius   :: !Radius
                 }
  deriving (Show)

type Update     = Boid -> Boid
type Perception = [Boid]
type Behaviour  = Perception -> Update
type Weights    = (Int,Int,Int)

emptyBehaviour :: Behaviour
            -- :: [Boid] -> Boid -> Boid
emptyBehaviour _ b = b

-- |'Behaviour' assuming all steer vectors
-- ('cohesion', 'separation', 'alignment') are equally weighted.
equalWeightsBehaviour :: Behaviour
equalWeightsBehaviour neighbors self = steer (1,1,1) neighbors self

-- |Compose a 'Behaviour' for a 'Boid' based on a tuple of 'Weights'.
steer :: Weights -> Behaviour
   -- :: Weights -> [Boid] -> Boid -> Boid
steer (s, c, m) neighbors self =
    let s_i  = (fromIntegral s) *^ (separation self neighbors)
        c_i  = (fromIntegral c) *^ (cohesion self neighbors)
        m_i  = (fromIntegral m) *^ (alignment self neighbors)
        v'   = (velocity self) ^+^ s_i ^+^ c_i ^+^ m_i
    in self { position = (position self) ^+^ v', velocity = v'}

positions :: [Boid] -> [Vector]
positions = map position


-- |Find the centre of a list of 'Boid's
centre :: [Boid] -> Vector
centre boids =
    let m = fromIntegral $ length boids :: Float
    in sumV $ map (^/ m) $ positions boids

-- |Find the separation steer vector for a 'Boid' given a 'Neighborhood'.
--
-- Separation steering force models the tendency of an individual to avoid
-- collisions with its flock-mates.
--
-- The separation steer vector /s/i can be computed simply by summing the
-- boid's position /p/i against each other position /p/j and taking the
-- negative sum of these vectors.
separation :: Boid -> [Boid] -> Vector
separation self neighbors =
    let p = position self
    in sumV . map (^-^ p) $ positions neighbors

-- |Find the cohesion steer vector for a 'Boid' given a 'Neighborhood'.
--
-- Cohesion steering force models the tendency of an individual to move
-- towards the centre of the flock. It is the counterpart to the 'separation'
-- steering force.
--
-- Cohesion is calculated in two steps. First, the centre /c/i is calculated
-- for the visible neighborhood. Then, the cohesion vector /k/i is calculated
-- by subtracting the current position /p/i from /c/i
cohesion :: Boid -> [Boid] -> Vector
cohesion self neighbors =
    let p = position self
    in centre neighbors - p

-- |Find the alignment steer vector for a boid given a neighborhood.
--
-- Alignment steering force models the tendency of an individual 'Boid'
-- to match velocities with its flock-mates. This force causes the 'Boid's to
-- speed up or slow down depending on the acceleration of nearby 'Boid's.
--
-- The alignment vector is calculated as the average of the velocity vectors
-- of this 'Boid''s 'Neighborhood'.
alignment :: Boid -> [Boid] -> Vector
alignment _ []           = V3 0 0 0
alignment self neighbors =
    let m = fromIntegral $ length neighbors :: Float
    in sumV $ map ((^/ m) . velocity) neighbors
