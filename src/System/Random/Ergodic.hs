module System.Random.Ergodic where

import qualified Data.ByteString.Lazy       as BS
import           System.Random              ( RandomGen ()
                                            , genWord32
                                            , split )
import           Data.Binary                ( encode )
import           Data.Ratio                 ( (%)
                                            , numerator
                                            , denominator )
import           Data.Word                  ( Word32 )
import           System.CPUTime             ( getCPUTime )
import           Data.RootX
import           System.Random.Xorshift
import           Data.WideWord              ( Word256 )
import           Control.Parallel.Strategies ( rpar
                                             , runEval )

instance RandomGen RootX where
    genWord32 gen = runEval $ do r    <- rpar $ mapIntRootX False 32 gen
                                 ngen <- rpar $ go gen
                                 return (r, ngen)
    --genWord32 gen = (mapIntRootX False 32 gen, go gen)
    --genWord32 gen = (xorshiftW32 10 (mapIntRootX False 32 gen), go gen)

--    split gen = (gen, gen)

-- move length = gr * (1 -/2)
-- -> lx * (1 -/2)
genRaw gen = (gen, go gen)

genWord256 gen = ((mapIntRootX False 256 gen), go gen)

genRational :: RootX -> (Rational, RootX)
genRational gen = (toRationalRootX gen, go gen)

getErgoGen :: IO RootX
getErgoGen =  mkErgoGen . fromIntegral <$> getCPUTime

mkErgoGen      :: Int -> RootX
mkErgoGen seed =  RootX (toRational ((xorshift seed) % maxBound)) 0
--mkErgoGen seed =  RootX (toInteger (abs (xorshift seed)) % maxBoundInt) 0

maxBoundInt :: Integer
maxBoundInt =  toInteger (maxBound :: Int)

lx :: RootX
lx =  RootX 0 1
--lx =  RootX (1 % 2) (1 % 2)
    --((1 % 2) -/1) + ((1 % 2) -/5)

ly :: RootX
ly =  1

ergoRandomsRaw     :: Int -> Int -> RootX
ergoRandomsRaw n s =  f' n s (mkErgoGen s) 0
                      where f' 0 _ _   x =  x
                            f' n s gen _ =  f' (n-1) s ngen w
                                            where (w, ngen) = genRaw gen

w32RandomsSum     :: Int -> Int -> Word32
w32RandomsSum n s =  f' n s (mkErgoGen s) 0
                     where f' 0 _ _   sumS = sumS
                           f' n s gen sumS =  f' (n-1) s ngen (sumS + w)
                                              where (w, ngen) = genWord32 gen

w256Randoms     :: Int -> Int -> [Word256]
w256Randoms n s =  f' n s (mkErgoGen s)
                   where f' 0 _ _   = []
                         f' n s gen =  w : f' (n-1) s ngen
                                       where (w, ngen) = genWord256 gen

exRaw        :: Int -> Int -> [RootX]
exRaw n seed =  f' n $ mkErgoGen seed
                where f'       :: Int -> RootX -> [RootX]
                      f' 0  _  =  []
                      f' n' s' =  ns : f' (n' - 1) ns
                                  where ns = go s'

exportData     :: (Eq a, Show a, Num a)
               => a    -- ^ Number of Random Numbers
               -> Int  -- ^ Seed
               -> Bool -- ^ True: Export CSV and Binary, False: Export Binary only
               -> IO ()
exportData n s =  export' ("UInt32_n-" ++ show n) 0 n (mkErgoGen s)

export'                :: (Eq a, Show a, Num a, RandomGen b, Show b)
                       => FilePath
                       -> a
                       -> a
                       -> b
                       -> Bool
                       -> IO ()
export' fn c n gen csv |  c == n    = return ()
                       |  csv       = e1 n gen fn c
                       |  otherwise = e2 n gen fn c
                                      where e1 n gen fn c |  c == n    = return ()
                                                          |  otherwise = do --putStrLn (show (c + 1) ++ " / " ++ show n)
                                                                            BS.appendFile (fn ++ ".bin") (encode r)
                                                                            appendFile    (fn ++ ".csv") (show r ++ ", " ++ show ngen ++ "\n")
                                                                            e1 n ngen fn (c + 1)
                                                                            where (r, ngen) =  genWord32 gen
                                            e2 n gen fn c |  c == n    = return ()
                                                          |  otherwise = do --putStrLn (show (c + 1) ++ " / " ++ show n)
                                                                            BS.appendFile (fn ++ ".bin") (encode r)
                                                                            e2 n ngen fn (c + 1)
                                                                            where (r, ngen) =  genWord32 gen

{-# INLINE mapIntRootX #-}
mapIntRootX       :: Integral a
                  => Bool       -- ^ Signed
                  -> Int        -- ^ Bits
                  -> RootX
                  -> a
mapIntRootX s i r =  floor ((toFloatingRootX r) * (mb s i))
                     where mb           :: Floating a => Bool -> Int -> a
                           mb True  8   =  127
                           mb True  16  =  32767
                           mb True  32  =  2147483647
                           mb True  64  =  9223372036854775807
                           mb False 8   =  255
                           mb False 16  =  65535
                           mb False 32  =  4294967295
                           mb False 64  =  18446744073709551615
                           mb False 256 =  115792089237316195423570985008687907853269984665640564039457584007913129639935

{-# INLINE go #-}
go      :: RootX -> RootX
go seed =  modRootX1 (addRtX seed)
--go seed =  modRootX1 (addRtX seed)
--go seed =  modRootX1 (seed + lx)
