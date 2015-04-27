module Main where

------------------------------------------------------------------------

import Boids
import Linear.Vector
import Linear.V3
import Linear.V2
import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.Pure.Simulate hiding (Point)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Control.Concurrent (threadDelay)
import Control.Lens -- sorry
import Control.Monad.Random
import System.Random
import Data.Fixed

------------------------------------------------------------------------

-- |A global world state is just a list of 'Boids'.
type World  = [Boid]

-- |Action update for a 'Boid'. The 'update' function should map this across the boids
type Action = World -> Boid -> Boid

-- |Update the entire world state by mapping an 'Action' across each 'Boid'
update :: Action -> World -> World
update a w = map (a w) w

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
initPos n = sequence $ replicate n $ (getRandomR (-50,50))

initWorld :: [(Float,Float)] -> World
initWorld = map mkBoid
  where mkBoid (x,y) = Boid (V2 x y) still rad
        still        = V2 0 0
        rad          = 50.0

norm :: Float -> Float -> Float
norm pos d = (pos / d) * 2 - (d / 2)

drawBoid :: Boid -> Picture
drawBoid (Boid (V2 xpos ypos) (V2 xvel yvel) rad) =
  Translate xpos ypos $
  Pictures [ (Circle 2)
           , (Color red $ Circle rad)
           , (Color green $ Line [(0,0), (xvel, yvel)])
           ]

drawWorld :: World -> Picture
drawWorld = Pictures . map drawBoid

boundsCheck :: (Int, Int) -> World -> World
boundsCheck (width, height) = map modBoid
  where modBoid b@(Boid (V2 x y) _ _) = b { position = V2 (x `mod'` width') (y `mod'` height') }
        width'  = fromIntegral width
        height' = fromIntegral height

advanceWorld :: (Int, Int) -> ViewPort -> Float -> World -> World
advanceWorld dims _ _ = (boundsCheck dims) . update swarmStep

main :: IO ()
main = do

  let dims = (800, 800)

  pos_x <- evalRandIO (initPos 20)
  pos_y <- evalRandIO (initPos 20)

  simulate (InWindow "Boids" dims (0, 0))
    (greyN 0.7)  -- background color
    30           -- updates per second
    (initWorld $ zip pos_x pos_y)
    drawWorld
    (advanceWorld dims)
