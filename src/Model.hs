module Model where

------------------------------------------------------------------------

import Boids
import Linear.V3
import Linear.V2
import Control.Lens
import Control.Monad
import Control.Monad.Random

------------------------------------------------------------------------

-- |A global world state is just a list of 'Boids'.
type World  = [Boid]

-- |Action update for a 'Boid'. The 'update' function should map this across
-- the boids
type Action = World -> Boid -> Boid

-- |Update the entire world state by mapping an 'Action' across each 'Boid'
update :: Action -> World -> World
update a w = map (a w) w

-- |Check if a point in a 3D space is within a given radius of
--  another 3D point.
inSphere :: V3 Float -> Radius -> V3 Float -> Bool
      -- :: V3 Float -> Float -> V3 Float -> Bool
inSphere p_0 r p_i = ((x_i - x)^n + (y_i - y)^n + (z_i - z)^n) <= r^n
    where x_i = p_i ^._x
          y_i = p_i ^._y
          z_i = p_i ^._z
          x   = p_0 ^._x
          y   = p_0 ^._y
          z   = p_0 ^._z
          n   = 2 :: Integer

-- |Check if a point in a 2D space is within a given radius of
--  another 2D point.
inCircle :: Point -> Radius -> Point -> Bool
      -- :: V2 Float -> Radius -> V2 Float -> Bool
inCircle p_0 r p_i = ((x_i - x)^n + (y_i - y)^n) <= r^n
  where x_i = p_i ^._x
        y_i = p_i ^._y
        x   = p_0 ^._x
        y   = p_0 ^._y
        n   = 2 :: Integer

-- |Find the neighborhood for a given 'Boid'
neighborhood :: World -> Boid -> Perception
          -- :: [Boid] -> Boid -> [Boid]
neighborhood world self = filter (inCircle cent rad . position) world
    where cent = position self
          rad  = radius self

emptyStep :: Action
emptyStep w b = emptyBehaviour (neighborhood w b) b

eqWeightStep :: Action
eqWeightStep w b = equalWeightsBehaviour (neighborhood w b) b

cohesiveStep :: Action
cohesiveStep w b = cohesiveBehaviour (neighborhood w b) b

swarmStep :: Action
swarmStep w b = swarmBehaviour (neighborhood w b) b

initPos :: (RandomGen g) => Int -> Rand g [Float]
initPos n = replicateM n $ getRandomR (-50,50)

initWorld :: [(Float,Float)] -> World
initWorld = map mkBoid
  where mkBoid (x,y) = Boid (V2 x y) still rad
        still        = V2 1 1
        rad          = 50.0

inBounds :: Float -> Float -> Float
inBounds bound = until (< bound) (subtract bound) . until (0 <=) (+ bound)

-- |Perform toroidal bounds checking on a 'World' given a height and width.
boundsCheck :: (Int, Int) -> World -> World
boundsCheck (width, height) = map modBoid
  where modBoid b@(Boid (V2 x y) _ _) = b { position = V2 (inWidth x) (inHeight y) }
        inWidth  = inBounds $ fromIntegral width
        inHeight = inBounds $ fromIntegral height