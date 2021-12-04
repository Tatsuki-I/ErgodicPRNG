module Main where

import Test.HTestU
import System.Random.Ergodic
import System.Random
import Test.HTestU.Streaming (nextStreamFromGen)
import Test.HTestU.Wrapping  (Battery)

runCrush :: (RandomGen g) => Battery -> IO g -> IO [TestResult]
runCrush crush gen = (\g -> runBatteryToResults nextStreamFromGen g crush ) `fmap` gen

runSmallCrushStd :: IO [TestResult]
runSmallCrushStd =  runCrush c_smallCrush newStdGen

runSmallCrushErgo :: IO [TestResult]
runSmallCrushErgo =  runCrush c_smallCrush getErgoGen

main :: IO ()
main =  do rStd <- runSmallCrushStd
           putStrLn "Standard"
           putStrLn $ show rStd
           rErgo <- runSmallCrushErgo
           putStrLn "Ergodic"
           putStrLn $ show rErgo


