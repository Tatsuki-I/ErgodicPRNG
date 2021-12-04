module Main where

import Test.HTestU
import System.Random.Ergodic
import System.Random
import System.Environment    ( getArgs )
import Test.HTestU.Streaming ( nextStreamFromGen )
import Test.HTestU.Wrapping  ( Battery )

runCrush :: (RandomGen g) => Battery -> IO g -> IO [TestResult]
runCrush crush gen = (\g -> runBatteryToResults nextStreamFromGen g crush ) `fmap` gen

runSmallCrushStd :: IO [TestResult]
runSmallCrushStd =  runCrush c_smallCrush newStdGen

runSmallCrushErgo :: IO [TestResult]
runSmallCrushErgo =  runCrush c_smallCrush getErgoGen

runCrushErgo :: IO [TestResult]
runCrushErgo =  runCrush c_crush getErgoGen

runBigCrushErgo :: IO [TestResult]
runBigCrushErgo =  runCrush c_bigCrush getErgoGen

main :: IO ()
main =  do args <- getArgs
           case (read $ args !! 0) of
                0 -> do rSCErgo <- runSmallCrushErgo
                        putStrLn "Ergodic Small Crush"
                        putStrLn $ show rSCErgo
                        putStrLn "\n"
                1 -> do rCErgo <- runCrushErgo
                        putStrLn "Ergodic Crush"
                        putStrLn $ show rCErgo
                        putStrLn "\n"
                _ -> do rBCErgo <- runBigCrushErgo
                        putStrLn "Ergodic Big Crush"
                        putStrLn $ show rBCErgo
                        putStrLn "\n"


           -- rStd <- runSmallCrushStd
           -- putStrLn "Standard Small Crush"
           -- putStrLn $ show rStd
           -- putStrLn "\n"
