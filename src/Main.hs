module Main where

------------------------------------------------------------------------

import Boids
import Model
import Linear.V2
import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.Pure.Simulate hiding (Point)
import Control.Monad.Random
import System.Environment( getArgs )
import System.Console.GetOpt

------------------------------------------------------------------------

data Options = Options
  { optDebug       :: Bool
  , optMode        :: Action
  }

defaultOptions  = Options
  { optDebug    = False
  , optMode     = eqWeightStep
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['d']     ["debug"]
      (NoArg (\ opts -> opts { optDebug = True}))
      "show debug mode"
  , Option ['c'] ["cohesive"]
      (NoArg (\ opts -> opts { optMode = cohesiveStep }))
      "cohesive behaviour"
  , Option ['s'] ["swarm"]
      (NoArg (\ opts -> opts { optMode = swarmStep }))
      "swarming behaviour"
  , Option ['e'] ["equal"]
      (NoArg (\ opts -> opts { optMode = eqWeightStep}))
      "equal weighted behavioir"
  ]

argOpts :: [String] -> IO (Options, [String])
argOpts argv =
  case getOpt Permute options argv of
     (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
 where header = "Usage: -dcsv"

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
advanceWorld dims _ _ = boundsCheck dims . update cohesiveStep

main :: IO ()
main = do

  let dims = (1000, 800)

  args <- getArgs
  let ( flags, nonOpts, msgs ) = getOpt RequireOrder options args

  pos_x <- evalRandIO (initPos 20)
  pos_y <- evalRandIO (initPos 20)

  simulate (InWindow "Boids" dims (0, 0))
    (greyN 0.7)  -- background color
    30           -- updates per second
    (initWorld $ zip pos_x pos_y)
    (drawWorld dims)
    (advanceWorld dims)
