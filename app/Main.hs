module Main where

import System.Environment ( getArgs )
import System.Random.Ergodic
import Data.Root

main :: IO ()
main =  do args <- getArgs
           case read (args !! 0) of
                0 -> print $ w32RandomsSum  (read (args !! 1)) (read (args !! 2))
                1 -> fastRandomSum          (read (args !! 1))
                2 -> print $ w32RandomsSumP (read (args !! 1)) (read (args !! 2))
                3 -> print $ w32XorshiftSum (read (args !! 1)) (read (args !! 2))
                _ -> exportData             (read (args !! 1)) (read (args !! 2)) False
--           putStrLn $ "Length is " ++ show (args !! 1)
--           putStrLn $ "Seed is "   ++ show (args !! 0)
--           exportData (read $ args !! 1)
--                      (read $ args !! 0)
--                      True
