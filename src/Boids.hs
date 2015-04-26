module Boids where

------------------------------------------------------------------------------

import Linear.V2
import Linear.Vector

------------------------------------------------------------------------------

type Vector = V2 Float
type Point  = V2 Float
type Radius = Float

data Boid = Boid { position :: !Point
                 , velocity :: !Vector
                 , radius   :: !Radius
                 }
  deriving (Show)

-- | An Update function maps a 'Boid' to a new 'Boid'.
type Update     = Boid -> Boid

-- | A 'Boid' percieves the environment as a list of 'Boid's.
type Perception = [Boid]

-- | A Behaviour function maps a 'Perception' of the environment to
--  a function for 'Update'ing a 'Boid''s position.
type Behaviour  = Perception -> Update

-- | Weight coefficients applied to control the influence of each
-- steer vector on a 'Boid'.
--
--      - The first coefficient, /S/, controls the weight of the
--        'separation' steer vector
--
--      - The second coefficient, /C/, controls the weight of the
--        'cohesion' steer vector
--
--      - The third coefficient, /M/, controls the weight of the
--        'alignment' steer vector
type Weights    = (Float,Float,Float)

emptyBehaviour :: Behaviour
            -- :: [Boid] -> Boid -> Boid
emptyBehaviour _ b = b

-- |'Behaviour' assuming all steer vectors
-- ('cohesion', 'separation', 'alignment') are equally weighted.
equalWeightsBehaviour :: Behaviour
equalWeightsBehaviour = steer (1.0,1.0,1.0)

-- |"Swarm behavior can be obtain by setting the velocity matching
-- (alignment) to zero." ~ Hartman and Beneš, p. 4
swarmBehaviour :: Behaviour
swarmBehaviour = steer (1.0,1.0,0.0)

cohesiveBehaviour :: Behaviour
cohesiveBehaviour = steer (2.0,1.0,1.0)

-- |Compose a 'Behaviour' for a 'Boid' based on a tuple of 'Weights'.
steer :: Weights -> Behaviour
   -- :: Weights -> [Boid] -> Boid -> Boid
steer (s, c, m) neighbors self =
    let s_i  = s *^ separation self neighbors
        c_i  = c *^ cohesion self neighbors
        m_i  = m *^ alignment self neighbors
        v'   = velocity self ^+^ s_i ^+^ c_i ^+^ m_i
        p    = position self
        p'   = p ^+^ v' -- TODO: a speed coefficient could be added here
    in self { position = p', velocity = v'}

-- | Extract the positions from a list of 'Boid's
positions :: Perception -> [Vector]
       -- :: [Boid] -> V2 Float
positions = map position


-- |Find the centre of a list of 'Boid's
centre :: Perception -> Vector
    -- :: [Boid] -> V2 Float
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
separation :: Boid -> Perception -> Vector
        -- :: Boid -> [Boid] -> V3 Float
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
cohesion :: Boid -> Perception -> Vector
      -- :: Boid -> [Boid] -> V2 Float
cohesion self neighbors =
    let p = position self
    in centre neighbors - p

-- |Find the alignment steer vector for a boid given a neighborhood.
--
-- Alignment steering force models the tendency of an individual 'Boid'
-- to match velocities with its flock-mates. This force causes the 'Boid's to
-- speed up or slow down depending on the acceleration of nearby 'Boid's.
--
-- The alignment vector is calculated as the average of the set of
-- velocity vectors (/V/) of this 'Boid''s 'neighborhood'. If the cardinality
-- of /V/ is 0, then the alignment vector is also 0.
alignment :: Boid -> Perception -> Vector
       -- :: Boid -> [Boid] -> V2 Float
alignment _ []        = V2 0 0
alignment _ neighbors =
    let m = fromIntegral $ length neighbors :: Float
    in (sumV $ map velocity neighbors) ^/ m
