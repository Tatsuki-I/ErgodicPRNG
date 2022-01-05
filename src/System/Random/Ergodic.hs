module System.Random.Ergodic where

import qualified Data.ByteString.Lazy        as BS
import           System.Random               ( RandomGen ()
                                             , genWord32
                                             , split )
import           Data.Binary                 ( encode )
import           Data.Ratio                  ( (%)
                                             , numerator
                                             , denominator )
import           Data.Int                    ( Int8
                                             , Int16
                                             , Int32
                                             , Int64 )
import           Data.Word                   ( Word8
                                             , Word16
                                             , Word32
                                             , Word64 )
import           System.CPUTime              ( getCPUTime )
import           Data.Irrational
import           System.Random.Xorshift
import           Data.WideWord               ( Word256 )
import           Control.Parallel.Strategies ( rpar
                                             , runEval )

instance RandomGen Irrational where
    genWord32 gen = runEval $ do r    <- rpar $ mapIntIr False 32 gen
                                 ngen <- rpar $ go gen
                                 return (r, ngen)
    --genWord32 gen = (mapIntIr False 32 gen, go gen)
    --genWord32 gen = (xorshiftW32 10 (mapIntIr False 32 gen), go gen)

--    split gen = (gen, gen)

-- move length = gr * (1 -/2)
-- -> lx * (1 -/2)
genRaw gen = (gen, go gen)

genWord256 gen = ((mapIntIr False 256 gen), go gen)

genRational :: Irrational -> (Rational, Irrational)
genRational gen = (toRationalIr gen, go gen)

getErgoGen :: IO Irrational
getErgoGen =  mkErgoGen . fromIntegral <$> getCPUTime

mkErgoGen      :: Int -> Irrational
mkErgoGen seed =  Irrational (toRational ((xorshift seed) % maxBound)) 0
--mkErgoGen seed =  Irrational (toInteger (abs (xorshift seed)) % maxBoundInt) 0

maxBoundInt :: Integer
maxBoundInt =  toInteger (maxBound :: Int)

lx :: Irrational
--lx =  Irrational 0 1
lx =  Irrational (1 % 2) (1 % 2)
    --((1 % 2) -/1) + ((1 % 2) -/5)

ly :: Irrational
ly =  1

ergoRandomsRaw     :: Int -> Int -> Irrational
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

exRaw        :: Int -> Int -> [Irrational]
exRaw n seed =  f' n $ mkErgoGen seed
                where f'       :: Int -> Irrational -> [Irrational]
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

{-# INLINE mapIntIr #-}
mapIntIr       :: Integral a
               => Bool       -- ^ Signed
               -> Int        -- ^ Bits
               -> Irrational
               -> a
mapIntIr s i r =  floor ((toFloatingIr r) * (mb s i))

{-# INLINE mb #-}
mb           :: Floating a => Bool -> Int -> a
mb True  8   =  fromIntegral (maxBound :: Int8) --127
mb True  16  =  fromIntegral (maxBound :: Int16) --32767
mb True  32  =  fromIntegral (maxBound :: Int32) --2147483647
mb True  64  =  fromIntegral (maxBound :: Int64) --9223372036854775807
mb False 8   =  fromIntegral (maxBound :: Word8) --255
mb False 16  =  fromIntegral (maxBound :: Word16) --65535
mb False 32  =  fromIntegral (maxBound :: Word32) --4294967295
mb False 64  =  fromIntegral (maxBound :: Word64) --18446744073709551615
mb False 256 =  fromIntegral (maxBound :: Word256) --115792089237316195423570985008687907853269984665640564039457584007913129639935

{-# INLINE go #-}
go      :: Irrational -> Irrational
go seed =  modIr1 (addIr seed)
--go seed =  modIr1 (addRtX seed)
--go seed =  modIr1 (seed + lx)

mkErgoGen' seed = Ergodic (mkErgoGen seed) True

data Ergodic = Ergodic Irrational
                       Bool
                       deriving ( Eq )

instance Show Ergodic where
  show (Ergodic i True ) = show i ++ ", Not flipped"
  show (Ergodic i False) = show i ++ ", Flipped"

instance RandomGen Ergodic where
    genWord32 gen@(Ergodic seed _) = (mapIntIr False 32 seed, next gen)

next     :: Ergodic -> Ergodic
next gen =  if inverse
               then Ergodic ns        True
               else Ergodic (ly - ns) False
             where Ergodic s b = gen
                   ln = lx
                   (ns, inverse)
                      =  if b
                            then ( modIr  (s + ln)
                                          ly
                                 , evenIr (divIr (s + ln)
                                                 ly) )
                            else ( modIr (ly - s + ln)
                                         ly
                                 , oddIr (divIr (ly - s + ln)
                                                 ly) )

{-
next     :: Ergodic -> Ergodic
next gen =  if wi
               then Ergodic (ly - ns) False
               else Ergodic ns        True
            where Ergodic s b = gen
                  ln = lx
                  ns = if b
                          then (s + ln)       `modIr` ly
                          else ly - ((s + ln) `modIr` ly)
                  wi :: Bool -- | will inverse
                  wi =  if (evenIr (ns `divIr` ly)) == b
                           then not b
                           else     b
-}


                           {-
                           next                      :: Ergodic -> Ergodic
                           next (Ergodic seed True)  |  g s'      = Ergodic s''        True  -- not inversed, will not inverse
                                                     |  otherwise = Ergodic (ly - s'') False -- not inversed, will inverse
                                                        where s'  = seed + ln
                                                              s'' = s' `modIr` ly
                           next (Ergodic seed False) |  g s'      = Ergodic (ly - s'') False -- inversed, will inverse
                                                     |  otherwise = Ergodic s''        True  -- inversed, will not inverse
                                                        where s'  = ly - seed + ln
                                                              s'' = s' `modIr` ly-}

genFloat :: Ergodic -> (Double, Ergodic)
genFloat gen@(Ergodic seed _) = (toFloatingIr seed, next gen)

exportData'     :: (Eq a, Show a, Num a)
                => a    -- ^ Number of Random Numbers
                -> Int  -- ^ Seed
                -> Bool -- ^ True: Export CSV and Binary, False: Export Binary only
                -> IO ()
exportData' n s =  export'' ("UInt32_n-" ++ show n) 0 n (mkErgoGen' s)


export''                :: (Eq a, Show a, Num a)
                        => FilePath
                        -> a
                        -> a
                        -> Ergodic
                        -> Bool
                        -> IO ()
export'' fn c n gen csv |  c == n    = return ()
                        |  csv       = e1 n gen fn c
                        |  otherwise = e2 n gen fn c
                                       where e1 n gen fn c |  c == n    = return ()
                                                           |  otherwise = do --putStrLn (show (c + 1) ++ " / " ++ show n)
                                                                             BS.appendFile (fn ++ ".bin") (encode r)
                                                                             appendFile    (fn ++ ".csv") (show r ++ ", " ++ show ngen ++ "\n")
                                                                             e1 n ngen fn (c + 1)
                                                                             where (r, ngen) =  genFloat gen
                                                                             --where (r, ngen) =  genWord32 gen
                                             e2 n gen fn c |  c == n    = return ()
                                                           |  otherwise = do --putStrLn (show (c + 1) ++ " / " ++ show n)
                                                                             BS.appendFile (fn ++ ".bin") (encode r)
                                                                             e2 n ngen fn (c + 1)
                                                                             where (r, ngen) =  genFloat gen
                                                                             --where (r, ngen) =  genWord32 gen
