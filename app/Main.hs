module Main where

import System.Environment ( getArgs )
import Data.ErgodicPRNG
import Data.Root

main :: IO ()
main =  do args <- getArgs
           appendCSVyIntBS (gr * (1-/2))
                           (read $ args !! 0)
                           0
                           0
           {-
           appendCSVyInt (gr * (1-/2))
                         (read $ args !! 0)
                         0
                         0
                         False
                         32 -}
