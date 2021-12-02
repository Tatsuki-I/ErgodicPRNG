module Main where

import System.Environment ( getArgs )
import Data.ErgodicPRNG

main :: IO ()
main =  do args <- getArgs
           appendCSV 10
                     1000000000
                     s1
                     s2
