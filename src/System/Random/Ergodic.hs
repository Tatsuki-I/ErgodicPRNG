module System.Random.Ergodic where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Internal as BI
import           System.Random           ( RandomGen ()
                                         , genWord32
                                         , split )
import           Data.Binary             ( encode )
import           Data.Ratio              ( (%)
                                         , numerator
                                         , denominator )
import           Data.Word               ( Word8
                                         , Word32 )
import           Data.Function           ( (&) )
import           Data.Bits               ( shiftL
                                         , shiftR
                                         , xor
                                         , (.|.) )
import           System.CPUTime          ( getCPUTime )
import           System.Entropy          ( getHardwareEntropy
                                         , getEntropy )
import           Network.Transport.Internal ( decodeWord32 )
import           GHC.Real                ( divZeroError )
import           Control.Parallel.Strategies ( rpar
                                             , runEval )
import           Data.Root
import           Data.WideWord           ( Word256 )

data RootX = RootX Rational
                   Rational -- Rational Term of -/2
                   deriving ( Eq )

data ParErgo = ParErgo [Word32]
                       ( RootX
                       , RootX
                       , RootX
                       , RootX )
                       deriving ( Show )

instance Show RootX where
    show (RootX 0 0) = "0"
    show (RootX 0 b) = "(" ++ show (numerator b) ++ " -/X) % " ++ show (denominator b)
    show (RootX a 0) = show a
    show (RootX a b) = show a ++ " + (" ++ show (numerator b) ++ " -/X) % " ++ show (denominator b)

instance Num RootX where
    {-# INLINE (+) #-}
    (RootX a b) + (RootX c d) = RootX (a + c) (b + d)

    {-# INLINE (*) #-}
    (RootX a b) * (RootX c d) = RootX (a * c + 2 * b * d) (c * b + a * d)

    {-# INLINE signum #-}
    signum r | r' > 0    =  1
             | r' < 0    = -1
             | otherwise =  0
               where r' = toRationalRootX r

    {-# INLINE negate #-}
    negate (RootX a b) = RootX (-a) (-b)

    {-# INLINE abs #-}
    abs r = r * signum r

    {-# INLINE fromInteger #-}
    fromInteger a = RootX (a % 1) 0

{-# INLINE toFloatingRootX #-}
toFloatingRootX :: Floating a => RootX -> a
toFloatingRootX =  fromRational . toRationalRootX

{-# INLINE toRationalRootX #-}
toRationalRootX             :: RootX -> Rational
toRationalRootX (RootX a b) =  a + b * rtX

instance Ord RootX where
    compare a b |  a == b              = EQ
                |  signum (a - b) == 1 = GT
                |  otherwise           = LT

{-# INLINE divRootX #-}
divRootX     :: RootX -> RootX -> RootX
divRootX a b |  a >= b = 1 + divRootX (a - b) b
             |  a <  b = 0

{-# INLINE modRootX #-}
modRootX     :: RootX -> RootX -> RootX
modRootX a b |  a == b = RootX 0 0
             |  a >  b = modRootX (a - b) b
             |  a <  b = a

{-# INLINE sub1 #-}
sub1               :: RootX -> RootX
sub1 (RootX r1 r2) =  RootX (r1 - 1) r2

{-# INLINE addRt2 #-}
addRt2               :: RootX -> RootX
addRt2 (RootX r1 r2) =  RootX r1 (r2 + 1)

{-# INLINE cpRootX1 #-}
cpRootX1           :: RootX -> Ordering
cpRootX1 (RootX 1 _)                        = EQ
cpRootX1 r         |  toRationalRootX r > 1 = GT
                   |  otherwise             = LT

{-# INLINE modRootX1 #-}
modRootX1   :: RootX -> RootX
modRootX1 a =  case cpRootX1 a of
                    GT -> modRootX1 (sub1 a)
                    LT -> a
                    EQ -> RootX 0 0

{-# INLINE rt5 #-}
rt5 :: Rational
-- rt5 =  2.236067977499789696 -- -/5
rt5 = 5374978561 % 2403763488 -- Continued Fraction(n=15)

{-# INLINE rt2 #-}
rt2 :: Rational
-- rt2 =  1.414213562373095048
rt2 = 4478554083 % 3166815962 -- Continued Fraction(n=25)

{-# INLINE rtX #-}
rtX :: Rational
rtX = rt2
--rtX =  2.2

instance RandomGen ParErgo where
    genWord32 (ParErgo [] ( r1
                          , r2
                          , r3
                          , r4 )) = runEval $ do (r1', gen1) <- rpar (mapIntRootX False 32 $ go r1, go r1)
                                                 (r2', gen2) <- rpar (mapIntRootX False 32 $ go r2, go r2)
                                                 (r3', gen3) <- rpar (mapIntRootX False 32 $ go r3, go r3)
                                                 (r4', gen4) <- rpar (mapIntRootX False 32 $ go r4, go r4)
                                                 return (r1', ParErgo [ r2'
                                                                      , r3'
                                                                      , r4']
                                                                      ( gen1
                                                                      , gen2
                                                                      , gen3
                                                                      , gen4 ))
    genWord32 (ParErgo (x : xs) gen) = (x, ParErgo xs gen)

instance RandomGen RootX where
    -- genWord32 gen = runEval $ do r    <- rpar $ mapIntRootX False 32 gen
    --                              ngen <- rpar $ go gen
    --                              return (r, ngen)
    genWord32 gen = ((mapIntRootX False 32 gen), go gen)

--    split gen = (gen, gen)

-- move length = gr * (1 -/2)
-- -> lx * (1 -/2)

genWord256 gen = ((mapIntRootX False 256 gen), go gen)

getErgoGen :: IO RootX
getErgoGen =  do cpu <- getCPUTime
                 return (mkErgoGen (fromIntegral cpu))

mkParErgo      :: Int -> ParErgo
mkParErgo seed =  ParErgo [] ( mkErgoGen s1
                             , mkErgoGen s2
                             , mkErgoGen s3
                             , mkErgoGen s4)
                  where s1 = xorshift seed
                        s2 = xorshift s1
                        s3 = xorshift s2
                        s4 = xorshift s3

mkErgoGen      :: Int -> RootX
mkErgoGen seed =  RootX (toRational (seed % maxBound)) 0
--mkErgoGen seed =  RootX (toInteger (abs (xorshift seed)) % maxBoundInt) 0

maxBoundInt :: Integer
maxBoundInt =  toInteger (maxBound :: Int)

xorshiftW32   :: Word32 -> Word32 -> Word32
xorshiftW32 0 s =  s
xorshiftW32 n s =  xorshiftW32 (n - 1) (s & (\v -> (v `shiftL` 13) `xor` v)
                                          & (\v -> (v `shiftR` 17) `xor` v)
                                          & (\v -> (v `shiftL` 15) `xor` v))

xorshiftW32'   :: Word32 -> Word32
xorshiftW32' s =  s & (\v -> (v `shiftL` 13) `xor` v)
                    & (\v -> (v `shiftR` 17) `xor` v)
                    & (\v -> (v `shiftL` 15) `xor` v)

xorshift   :: Int -> Int
xorshift s =  s & (\v -> (v `shiftL` 23) `xor` v)
                & (\v -> (v `shiftR` 13) `xor` v)
                & (\v -> (v `shiftL` 58) `xor` v)

lx :: RootX
lx =  RootX 0 1
--lx =  RootX (1 % 2) (1 % 2)
    --((1 % 2) -/1) + ((1 % 2) -/5)

ly :: RootX
ly =  1

w32XorshiftSum     :: Word32 -> Word32 -> Word32
w32XorshiftSum n s =  f' n s 0
                      where f' 0 _ sumS = sumS
                            f' n s sumS =  f' (n-1) ns (sumS + ns)
                                           where ns = xorshiftW32' s

w32RandomsSum     :: Int -> Int -> Word32
w32RandomsSum n s =  f' n s (mkErgoGen s) 0
                     where f' 0 _ _   sumS = sumS
                           f' n s gen sumS =  f' (n-1) s ngen (sumS + w)
                                              where (w, ngen) = genWord32 gen

w32RandomsSumP     :: Int -> Int -> Word32
w32RandomsSumP n s =  f' n s (mkParErgo s) 0
                      where f' 0 _ _   sumS = sumS
                            f' n s gen sumS =  f' (n-1) s ngen (sumS + w)
                                               where (w, ngen) = genWord32 gen

w32Randoms     :: Int -> Int -> [Word32]
w32Randoms n s =  f' n s (mkParErgo s)
--w32Randoms n s =  f' n s (mkErgoGen s)
                  where f' 0 _ _   = []
                        f' n s gen =  w : f' (n-1) s ngen
                                      where (w, ngen) = genWord32 gen

w256Randoms     :: Int -> Int -> [Word256]
w256Randoms n s =  f' n s (mkErgoGen s)
                   where f' 0 _ _   = []
                         f' n s gen =  w : f' (n-1) s ngen
                                       where (w, ngen) = genWord256 gen

exRaw        :: Int -> RootX -> [RootX]
exRaw 0 _    =  []
exRaw n seed =  ns : exRaw (n - 1) ns
                             where ns = go seed

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
                                                          |  otherwise = do putStrLn (show (c + 1) ++ " / " ++ show n)
                                                                            BS.appendFile (fn ++ ".bin") (encode r)
                                                                            appendFile    (fn ++ ".csv") (show r ++ ", " ++ show ngen ++ "\n")
                                                                            e1 n ngen fn (c + 1)
                                                                            where (r, ngen) =  genWord32 gen
                                            e2 n gen fn c |  c == n    = return ()
                                                          |  otherwise = do putStrLn (show (c + 1) ++ " / " ++ show n)
                                                                            BS.appendFile (fn ++ ".bin") (encode r)
                                                                            e2 n ngen fn (c + 1)
                                                                            where (r, ngen) =  genWord32 gen

{-# INLINE mapIntRootX #-}
mapIntRootX        :: Integral a
                   => Bool       -- ^ Signed
                   -> Int        -- ^ Bits
                   -> RootX
                   -> a
mapIntRootX s i r =  floor (toFloatingRootX (r * mb s i))
                     where mb         :: Bool -> Int -> RootX
                           mb True  8  =  127
                           mb True  16 =  32767
                           mb True  32 =  2147483647
                           mb True  64 =  9223372036854775807
                           mb False 8  =  255
                           mb False 16 =  65535
                           mb False 32 =  4294967295
                           mb False 64 =  18446744073709551615
                           mb False 256 =  115792089237316195423570985008687907853269984665640564039457584007913129639935

{-# INLINE go #-}
go      :: RootX -> RootX
go seed =  modRootX1 (addRt2 seed)
--go seed =  modRootX1 (seed + lx)

fastRandom nr = do s <- maybe (getEntropy nr) pure =<< getHardwareEntropy (nr * 4)
                   print $ w8ToW32 $ BI.unpackBytes s

w8ToW32                          :: [Word8] -> [Word32]
w8ToW32 []                       =  []
w8ToW32 (w1 : w2 : w3 : w4 : ws) =  w8ToW32' (w1, w2, w3, w4) : w8ToW32 ws

fastRandomSum nr = do s <- maybe (getEntropy nr) pure =<< getHardwareEntropy (nr * 4)
                      print $ w8ToW32Sum (BI.unpackBytes s) 0

w8ToW32Sum                               :: [Word8] -> Word32 -> Word32
w8ToW32Sum []                       sumS =  sumS
w8ToW32Sum (w1 : w2 : w3 : w4 : ws) sumS =  w8ToW32Sum ws (sumS + w8ToW32' (w1, w2, w3, w4))

w8ToW32'                  :: (Word8, Word8, Word8, Word8) -> Word32
w8ToW32' (w1, w2, w3, w4) =  (((((w1' `shiftL` 8) .|.  w2') `shiftL` 8) .|. w3') `shiftL` 8) .|. w4'
                             where w1' :: Word32
                                   w1' =  fromIntegral w1
                                   w2' :: Word32
                                   w2' =  fromIntegral w2
                                   w3' :: Word32
                                   w3' =  fromIntegral w3
                                   w4' :: Word32
                                   w4' =  fromIntegral w4
