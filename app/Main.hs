module Main where

import System.Environment ( getArgs )
import System.Random.Ergodic
import Data.Root

main :: IO ()
main =  do args <- getArgs
           putStrLn $ "Length is " ++ show (args !! 1)
           putStrLn $ "Seed is "   ++ show (args !! 0)
           exportData (read $ args !! 1)
                      (read $ args !! 0)
                      True
