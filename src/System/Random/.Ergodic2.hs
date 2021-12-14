module System.Random.Ergodic2 where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Internal as BI
import           System.Random           ( RandomGen ()
                                         , genWord32
                                         , split )
import           Data.Binary             ( encode )
import           Data.Ratio              ( (%) )
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

data Root2 = Root2 Rational'
                   Rational'
                   deriving ( Eq )

data Rational' = Integer :%% Integer
                 deriving ( Show
                          , Eq )

(%%)   :: Integer -> Integer -> Rational'
a %% b |  b >  0 = a          :%% b
       |  b <  0 = (negate a) :%% (negate b)
       |  b == 0 = divZeroError

-- *** Exception: Ratio has zero denominator
instance Num Rational' where
    (a :%% b) + (c :%% d) = (a * d + b * c) %% (b * d)
    (a :%% b) * (c :%% d) = (a * c) %% (b * d)

    negate (a :%% b) = (- a) %% b

    signum (a :%% b) | a > 0     =   1
                     | a < 0     = - 1
                     | otherwise =   0

    abs r@(a :%% b) | a >= 0    = r
                    | otherwise = negate r

    fromInteger a = a %% 1

numerator           :: Rational' -> Integer
numerator (a :%% _) =  a

denominator           :: Rational' -> Integer
denominator (_ :%% b) =  b

instance Ord Rational' where
    compare a b | a == b              = EQ
                | signum (a - b) == 1 = GT
                | otherwise           = LT

instance Real Rational' where
    toRational (a :%% b) =  a % b

rt2 :: Rational'
rt2 =  7 %% 5

-- rt2 :: Rational
-- rt2 =  1.4

-- rt2 :: Rational
-- rt2 =  1.414213562373095048

instance Show Root2 where
    show (Root2 0 0) = "0"
    show (Root2 0 b) = "(" ++ show (numerator b) ++ " -/2) % " ++ show (denominator b)
    show (Root2 a 0) = show a
    show (Root2 a b) = show a ++ " + (" ++ show (numerator b) ++ " -/2) % " ++ show (denominator b)

instance Num Root2 where
    (Root2 a b) + (Root2 c d) = Root2 (a + c) (b + d)

    (Root2 a b) * (Root2 c d) = Root2 (a * c + 2 * b * d) (c * b + a * d)

    signum r | fr > 0    =  1
             | fr < 0    = -1
             | otherwise =  0
               where fr = toFloatingRoot2 r

    negate (Root2 a b) = Root2 (-a) (-b)

    abs r = r * signum r

    fromInteger a = Root2 (fromInteger a) 0

toW32Root2             :: Root2 -> Word32
toW32Root2 (Root2 a b) =  fromInteger (n `div` d)
                          where mbW32 = 4294967295
                                (n :%% d) = (a + b * rt2) * mbW32

toFloatingRoot2             :: Floating a => Root2 -> a
toFloatingRoot2 (Root2 a b) =  fromRational $ toRational $ a + b * rt2

instance Ord Root2 where
    compare a b |  a == b              = EQ
                |  signum (a - b) == 1 = GT
                |  otherwise           = LT

divRoot2     :: Root2 -> Root2 -> Root2
divRoot2 a b |  a >= b = 1 + divRoot2 (a - b) b
             |  a <  b = 0

modRoot2     :: Root2 -> Root2 -> Root2
modRoot2 a b |  a == b = Root2 0 0
             |  a >  b = modRoot2 (a - b) b
             |  a <  b = a

evenRoot2   :: Root2 -> Bool
evenRoot2 r =  (r `modRoot2` 2) == 0

oddRoot2   :: Root2 -> Bool
oddRoot2 r =  not (evenRoot2 r)

sr' :: Root2
sr' =  Root2 0 1

instance RandomGen Root2 where
    genWord32 gen = (toW32Root2 ngen, ngen)
    --genWord32 gen = ((mapIntRoot2 False 32 s), ngen)
                    where ngen = go gen

--    split gen = (gen, gen)

-- move length = gr * (1 -/2)
-- -> lx * (1 -/2)

getErgoGen :: IO Root2
getErgoGen =  do cpu <- getCPUTime
                 return (mkErgoGen (fromIntegral cpu))

mkErgoGen      :: Int -> Root2
mkErgoGen seed =  Root2 (toInteger (abs (xorshift seed)) %% maxBoundInt) 0

maxBoundInt :: Integer
maxBoundInt =  toInteger (maxBound :: Int)

xorshiftW32   :: Word32 -> Word32 -> Word32
xorshiftW32 0 s =  s
xorshiftW32 n s =  xorshiftW32 (n - 1) (s & (\v -> (v `shiftL` 13) `xor` v)
                                          & (\v -> (v `shiftR` 17) `xor` v)
                                          & (\v -> (v `shiftL` 15) `xor` v))

xorshift   :: Int -> Int
xorshift s =  s & (\v -> (v `shiftL` 23) `xor` v)
                & (\v -> (v `shiftR` 13) `xor` v)
                & (\v -> (v `shiftL` 58) `xor` v)

lx :: Root2
lx =  sr'
ly :: Root2
ly =  1

w32Randoms     :: Int -> Int -> [Word32]
w32Randoms n s =  f' n s (mkErgoGen s)
                  where f' 0 _ _   = []
                        f' n s gen =  w : f' (n-1) s ngen
                                      where (w, ngen) = genWord32 gen

exRaw                     :: Int -> Root2 -> [Root2]
exRaw 0 _   =  []
exRaw n gen =  ngen : exRaw (n - 1) ngen
               where ngen = go gen

exportData     :: (Eq a, Show a, Num a)
               => a    -- ^ Number of Random Numbers
               -> Int  -- ^ Seed
               -> Bool -- ^ True: Export CSV and Binary, False: Export Binary only
               -> IO ()
exportData n s =  export' ("UInt32_n-" ++ show n) 0 n (mkErgoGen s)

export'                :: (Eq a, Show a, Num a, RandomGen b)
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
                                                                            appendFile    (fn ++ ".csv") (show r ++ "\n")
                                                                            e1 n ngen fn (c + 1)
                                                                            where (r, ngen) =  genWord32 gen
                                            e2 n gen fn c |  c == n    = return ()
                                                          |  otherwise = do putStrLn (show (c + 1) ++ " / " ++ show n)
                                                                            BS.appendFile (fn ++ ".bin") (encode r)
                                                                            e2 n ngen fn (c + 1)
                                                                            where (r, ngen) =  genWord32 gen

mapIntRoot2        :: Integral a
                   => Bool       -- ^ Signed
                   -> Int        -- ^ Bits
                   -> Root2
                   -> a
mapIntRoot2 s i r =  floor (toFloatingRoot2 (r * mb s i))
                     where mb         :: Bool -> Int -> Root2
                           mb True  8  =  127
                           mb True  16 =  32767
                           mb True  32 =  2147483647
                           mb True  64 =  9223372036854775807
                           mb False 8  =  255
                           mb False 16 =  65535
                           mb False 32 =  4294967295
                           mb False 64 =  18446744073709551615

go                    :: Root2 -> Root2
go gen = (gen + lx) `modRoot2` 1
                                         {-
go                    :: Ergodic -> Ergodic
go (Ergodic seed cby) =  Ergodic ny nby
                         where ln        = lx
                               (ny, nby) | cby && -- Y is not inversed
                                           evenRoot2 (divRoot2 (seed + ln) -- Y will not inverse
                                                             ly) = ((seed + ln) `modRoot2` ly,  True)
                                         | cby  -- X is not inversed
                                                                 = (ly - ((seed + ln) `modRoot2` ly), False)
                                         | not cby && -- X is inversed
                                           oddRoot2 (divRoot2 (ly - seed + ln) -- X will not inverse
                                                            ly)  = ((ly - seed + ln) `modRoot2` ly,  True)
                                         | otherwise             = (ly - ((ly - seed + ln) `modRoot2` ly),  False)

go     :: Ergodic -> Ergodic
go rst =  rst { _x     = nx
              , _y     = ny
              , _bx    = nbx
              , _by    = nby
              , _count = rst ^. count + 1 }
          where --ln  = (rst ^. l) * ((1 % 2) -/ 2)
                ln  = (lx * (1 -/ 2)) * ((1 % 2) -/ 2)
                cx  = rst ^. x  -- current x
                cy  = rst ^. y  -- current y
                cbx = rst ^. bx -- current x bool
                cby = rst ^. by -- current y bool
                (nx, nbx) | cbx && -- X is not inversed
                            evenRoot (divRoot (cx + ln) -- X will not inverse
                                              (lx)) = ((cx + ln) `modRoot` lx,  True)
                          | cbx  -- X is not inversed
                                                    = (lx - ((cx + ln) `modRoot` lx), False)
                          | not cbx && -- X is inversed
                            oddRoot (divRoot (lx - cx + ln) -- X will not inverse
                                             (lx))  = ((lx - cx + ln) `modRoot` lx,  True)
                          | otherwise               = (lx - ((lx - cx + ln) `modRoot` lx),  False)
                (ny, nby) | cby && -- Y is not inversed
                            evenRoot (divRoot (cy + ln) -- Y will not inverse
                                              (ly)) = ((cy + ln) `modRoot` ly,  True)
                          | cby  -- X is not inversed
                                                    = (ly - ((cy + ln) `modRoot` ly), False)
                          | not cby && -- X is inversed
                            oddRoot (divRoot (ly - cy + ln) -- X will not inverse
                                             (ly))  = ((ly - cy + ln) `modRoot` ly,  True)
                          | otherwise               = (ly - ((ly - cy + ln) `modRoot` ly),  False)
-}

fastRandom nr = do s <- maybe (getEntropy nr) pure =<< getHardwareEntropy (nr * 4)
                   print $ w8ToW32 $ BI.unpackBytes s

w8ToW32                          :: [Word8] -> [Word32]
w8ToW32 []                       =  []
w8ToW32 (w1 : w2 : w3 : w4 : ws) =  w8ToW32' (w1, w2, w3, w4) : w8ToW32 ws

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
