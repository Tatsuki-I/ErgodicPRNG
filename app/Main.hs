module Main where

import System.Environment ( getArgs )
import Data.ErgodicPRNG
import Data.Root

main :: IO ()
main =  do args <- getArgs
           appendCSVyInt (gr * (1-/2))
                         1000000000
                         0
                         0
                         32
