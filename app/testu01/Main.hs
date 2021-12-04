module Main where

import Test.HTestU
import System.Random.Ergodic
import System.Random         ( randoms )

main :: IO ()
main =  do print "Hello"
           print $ take 100 rs
           print ds
           print (toResults ds)
           where ds = runBattery (System.Random.randoms) (mkErgoGen 111) c_crush
           -- where ds = runBattery (System.Random.randoms) (mkErgoGen 111) c_bigCrush
                 rs :: [Int]
                 rs =  System.Random.randoms (mkErgoGen 111)


