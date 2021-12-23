module Main where

import System.Environment ( getArgs )
import System.Random.Ergodic
import System.Random.RDRAND
import System.Random.Xorshift
import Data.Root
import Crypto.Random.Entropy

main :: IO ()
main =  do args <- getArgs
           case read (args !! 0) of
                0 ->  do putStrLn "Ergodic"
                         print $ w32RandomsSum   (read (args !! 1)) (read (args !! 2))
                1 ->  do putStrLn "RDRAND"
                         fastRandomSum           (read (args !! 1))
                2 ->  do putStrLn "Ergodic Flip"
                3 ->  do putStrLn "Xorshift"
                         print $ w32XorshiftLast (read (args !! 1)) (read (args !! 2))
                4 ->  do putStrLn "Ergodic Raw"
                         print $ ergoRandomsRaw  (read (args !! 1)) (read (args !! 2))
                5 ->  do putStrLn "Ergodic Export"
                         exportData              (read (args !! 1)) (read (args !! 2)) False
                _ ->  do putStrLn "Ergodic Flip Export"
                         exportData'             (read (args !! 1)) (read (args !! 2)) True
--           putStrLn $ "Length is " ++ show (args !! 1)
--           putStrLn $ "Seed is "   ++ show (args !! 0)
--           exportData (read $ args !! 1)
--                      (read $ args !! 0)
--                      True
