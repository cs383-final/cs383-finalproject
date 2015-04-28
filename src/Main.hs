module Main where

-------------------------------------------------------------------------------

import Boids
import Model
import Linear.V2
import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.Pure.Simulate hiding (Point)
import Control.Monad.Random
import System.Environment( getArgs )
import System.Console.GetOpt
import System.Exit

-------------------------------------------------------------------------------

-- Option parsing stuff -------------------------------------------------------
data Options = Options
  { optDrawMode    :: BoidArtist
  , optStep        :: Step
  , optHeight      :: Int
  , optWidth       :: Int
  , optNumber      :: Int
  , optRadius      :: Float
  , optSpeed       :: Float
  }

-- Some sensible default configurations
defaultOptions :: Options
defaultOptions  = Options
  { optDrawMode = drawPretty
  , optStep     = eqWeightStep
  , optHeight   = 800
  , optWidth    = 800
  , optNumber   = 20
  , optRadius   = 50.0
  , optSpeed    = 1000.0
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option ['d']     ["debug"]
      (NoArg (\ opts -> return opts { optDrawMode = drawDebug }))
      "Draw boids in debug mode"
  , Option ['c'] ["cohesive"]
      (NoArg (\ opts -> return opts { optStep = cohesiveStep }))
      "Cohesive boid behaviour"
  , Option ['s'] ["swarm"]
      (NoArg (\ opts -> return opts { optStep = swarmStep }))
      "Swarming boid behaviour"
  , Option ['e'] ["equal"]
      (NoArg (\ opts -> return opts { optStep = eqWeightStep}))
      "Equal-weighted boid behaviour"
  , Option ['x'] ["height"]
      (ReqArg (\x opts -> return opts { optHeight = read x :: Int}) "HEIGHT")
      "Window height (pixels)"
  , Option ['y'] ["width"]
      (ReqArg (\x opts -> return opts { optWidth = read x :: Int}) "WIDTH")
      "Window width (pixels)"
  , Option ['n'] ["num"]
      (ReqArg (\x opts -> return opts { optNumber = read x :: Int}) "BOIDS")
      "Number of boids in the simulation"
  , Option ['v'] ["visibility"]
      (ReqArg (\x opts -> return opts { optRadius = read x :: Float}) "RADIUS")
      "Boid visibility radius\nDefault is 50"
  , Option ['p'] ["speed"]
      (ReqArg (\x opts -> return opts { optSpeed = read x :: Float}) "SPEED")
      "Higher values make boids move slower.\nDefault is 1000 at 30fps."
  , Option "h" ["help"]
        (NoArg
            (\_ -> do
                putStrLn (usageInfo "Boids" options)
                exitWith ExitSuccess))
        "Show this help file"
  ]

-- View ---------------------------------------------------------------------

type BoidArtist = (Float, Float) -> Boid -> Picture

drawPretty :: BoidArtist
drawPretty dims boid = case boid of
  (Boid (V2 xpos ypos) _ _) -> Translate xpos ypos $ drawBoid dims boid

drawBoid :: BoidArtist
drawBoid (xtrans, ytrans) (Boid (V2 xpos ypos) (V2 xvel yvel) _) =
  Rotate theta $ Polygon [(-6,0), (0,3), (6,0)]
  where theta = toDegrees $ atan2 xdiff ydiff
        xdiff = (xvel + xtrans) - (xpos + xtrans)
        ydiff = (yvel + ytrans) - (ypos + ytrans)
        toDegrees rad = rad * 180 / pi

drawDebug :: BoidArtist
drawDebug dims boid = case boid of
  (Boid (V2 xpos ypos) (V2 xvel yvel) rad) ->
    Translate xpos ypos $
    Pictures [ drawBoid dims boid
             , Color red $ Circle rad
             , Color green $ Line [(0,0), (xvel, yvel)]
             ]

drawWorld :: (Int, Int) -> BoidArtist -> World -> Picture
drawWorld (xdim, ydim) draw = Translate xtrans ytrans . Pictures . map (draw (xtrans, ytrans))
  where xtrans = - fromIntegral xdim / 2
        ytrans = - fromIntegral ydim / 2

-- Main simulation loop -------------------------------------------------------

advanceWorld :: (Int, Int) -> Action -> ViewPort -> Float -> World -> World
advanceWorld dims step _ _ = boundsCheck dims . update step

main :: IO ()
main = do
  args <- getArgs

  -- Parse options, getting a list of option actions
  let (actions, _, _) = getOpt RequireOrder options args

  -- Here we thread startOptions through all supplied option actions
  opts <- foldl (>>=) (return defaultOptions) actions

  let Options { optDrawMode = mode
              , optStep     = step
              , optHeight   = height
              , optWidth    = width
              , optNumber   = number
              , optRadius   = rad
              , optSpeed    = speed
              } = opts

  let dims = (height, width)

  pos_x <- evalRandIO (initPos ((fromIntegral height :: Float)/2) number)
  pos_y <- evalRandIO (initPos ((fromIntegral width :: Float)/2) number)

  simulate (InWindow "Boids" dims (0, 0))
    (greyN 0.7)  -- background color
    30           -- updates per second
    (initWorld rad $ zip pos_x pos_y)
    (drawWorld dims mode)
    (advanceWorld dims (step speed))
