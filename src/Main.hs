module Main where
------------------------------------------------------------------------
import Boid
------------------------------------------------------------------------
type World  = [Boid]
type Update = World -> World

main :: IO ()
main = putStrLn "This program needs to be written :("
