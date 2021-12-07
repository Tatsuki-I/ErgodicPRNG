{-# LANGUAGE Safe #-}

module System.Random.Ergodic where

import qualified Data.ByteString.Lazy as BS
import           System.Random           ( RandomGen ()
                                         , genWord32
                                         , split )
import           Data.Root               ( Root ()
                                         , (-/)
                                         , modRoot
                                         , divRoot
                                         , oddRoot
                                         , evenRoot
                                         , toFloating
                                         , gr
                                         , sr )
import           Data.Binary             ( encode )
import           Data.Ratio              ( (%)
                                         , numerator
                                         , denominator )
import           Data.Word               ( Word32 )
import           Data.Function           ( (&) )
import           Data.Bits               ( shiftL
                                         , shiftR
                                         , xor )
import           System.CPUTime          ( getCPUTime )

data Root2 = Root2 Rational
                   Rational
                   deriving ( Eq )

rt2 :: Rational
rt2 =  1.414213562373095048

instance Show Root2 where
    show (Root2 0 0) = "0"
    show (Root2 0 b) = "(" ++ show (numerator b) ++ " -/2) % " ++ show (denominator b)
    show (Root2 a 0) = show a
    show (Root2 a b) = show a ++ " + (" ++ show (numerator b) ++ " -/2) % " ++ show (denominator b)

instance Num Root2 where
    (Root2 na da) + (Root2 nb db) = Root2 (na + nb) (da + db)

    (Root2 na da) * (Root2 nb db) = Root2 (na * nb + 2 * da * db) (na * db + nb * da)

    signum r | fr > 0    =  1
             | fr < 0    = -1
             | otherwise =  0
               where fr = toFloatingRoot2 r

    negate (Root2 a b) = Root2 (-a) (-b)

    abs r = r * signum r

    fromInteger a = Root2 (a % 1) 0

toFloatingRoot2 :: Floating a => Root2 -> a
toFloatingRoot2 (Root2 a b) =  fromRational $ a * b * rt2

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
sr' = Root2 0 1

data Ergodic = Ergodic Root Bool

instance RandomGen Ergodic where
    genWord32 gen = (xorshiftW32 5 (mapInt False 32 s), ngen)
                    where ngen@(Ergodic s _) = go gen

--    split gen = (gen, gen)

-- move length = gr * (1 -/2)
-- -> lx * (1 -/2)

getErgoGen :: IO Ergodic
getErgoGen =  do cpu <- getCPUTime
                 return (mkErgoGen (fromIntegral cpu))

mkErgoGen      :: Int -> Ergodic
mkErgoGen seed =  Ergodic ((abs (xorshift seed) % maxBound) -/ 1)
                          True

xorshiftW32   :: Word32 -> Word32 -> Word32
xorshiftW32 0 s =  s
xorshiftW32 n s =  xorshiftW32 (n - 1) (s & (\v -> (v `shiftL` 13) `xor` v)
                                          & (\v -> (v `shiftR` 17) `xor` v)
                                          & (\v -> (v `shiftL` 15) `xor` v))

xorshift   :: Int -> Int
xorshift s =  s & (\v -> (v `shiftL` 23) `xor` v)
                & (\v -> (v `shiftR` 13) `xor` v)
                & (\v -> (v `shiftL` 58) `xor` v)

s1 = (1 % 2) -/ 1
s2 = (1 % 2) -/ 1

lx :: Root
lx =  sr
ly :: Root
ly =  1

re = ((3 % 2) -/1) + ((-1 % 2) -/5) -- 1/2.618

exRaw                     :: Int -> Ergodic -> [Root]
exRaw 0 s                 =  []
exRaw n gen@(Ergodic s b) =  ns : exRaw (n - 1) ngen
                             where ngen@(Ergodic ns _) = go gen

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

mapInt       :: Integral a
             => Bool       -- ^ Signed
             -> Int        -- ^ Bits
             -> Root
             -> a
mapInt s i r =  floor (toFloating (r * mb s i))
                 where mb         :: Bool -> Int -> Root
                       mb True  8  =  127
                       mb True  16 =  32767
                       mb True  32 =  2147483647
                       mb True  64 =  9223372036854775807
                       mb False 8  =  255
                       mb False 16 =  65535
                       mb False 32 =  4294967295
                       mb False 64 =  18446744073709551615

go                    :: Ergodic -> Ergodic
go (Ergodic seed cby) =  Ergodic ny nby
                         where ln   = (lx * (1 -/ 2)) * ((1 % 2) -/ 2)
                               (ny, nby) | cby && -- Y is not inversed
                                           evenRoot (divRoot (seed + ln) -- Y will not inverse
                                                             ly) = ((seed + ln) `modRoot` ly,  True)
                                         | cby  -- X is not inversed
                                                                   = (ly - ((seed + ln) `modRoot` ly), False)
                                         | not cby && -- X is inversed
                                           oddRoot (divRoot (ly - seed + ln) -- X will not inverse
                                                            ly)  = ((ly - seed + ln) `modRoot` ly,  True)
                                         | otherwise               = (ly - ((ly - seed + ln) `modRoot` ly),  False)

{-
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
