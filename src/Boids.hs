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
