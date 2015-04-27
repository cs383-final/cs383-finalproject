module Main where

------------------------------------------------------------------------

import Boids
import Model
import Linear.V3
import Linear.V2
import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.Pure.Simulate hiding (Point)
import Control.Monad.Random
import Control.Monad

------------------------------------------------------------------------


norm :: Float -> Float -> Float
norm pos d = (pos / d) * 2 - (d / 2)

drawBoid :: Boid -> Picture
drawBoid (Boid (V2 xpos ypos) (V2 xvel yvel) rad) =
  Translate xpos ypos $
  Pictures [ Circle 2
           , Color red $ Circle rad
           , Color green $ Line [(0,0), (xvel, yvel)]
           ]

drawWorld :: (Int, Int) -> World -> Picture
drawWorld (xdim, ydim) = Translate xtrans ytrans . Pictures . map drawBoid
  where xtrans = - fromIntegral xdim / 2
        ytrans = - fromIntegral ydim / 2

inBounds :: Float -> Float -> Float
inBounds bound = until (< bound) (subtract bound) . until (0 <=) (+ bound)

boundsCheck :: (Int, Int) -> World -> World
boundsCheck (width, height) = map modBoid
  where modBoid b@(Boid (V2 x y) _ _) = b { position = V2 (inWidth x) (inHeight y) }
        inWidth  = inBounds $ fromIntegral width
        inHeight = inBounds $ fromIntegral height

advanceWorld :: (Int, Int) -> ViewPort -> Float -> World -> World
advanceWorld dims _ _ = boundsCheck dims . update swarmStep

main :: IO ()
main = do

  let dims = (1000, 800)

  pos_x <- evalRandIO (initPos 20)
  pos_y <- evalRandIO (initPos 20)

  simulate (InWindow "Boids" dims (0, 0))
    (greyN 0.7)  -- background color
    30           -- updates per second
    (initWorld $ zip pos_x pos_y)
    (drawWorld dims)
    (advanceWorld dims)
