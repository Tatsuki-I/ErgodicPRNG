module Data.RootX where

import           Data.Ratio                 ( (%) )

data RootX = RootX Rational
                   Integer -- Integer Term of -/2
                   deriving ( Eq )

instance Show RootX where
    {-# INLINE show #-}
    show (RootX 0 0) = "0"
    show (RootX 0 b) = show b ++ " -/X"
    show (RootX a 0) = show a
    show (RootX a b) = show a ++ " + " ++ show b ++ " -/X"

instance Num RootX where
    {-# INLINE (+) #-}
    (RootX a b) + (RootX c d) = RootX (a + c) (b + d)

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
toRationalRootX (RootX a b) =  a + (toRational b) * rtX

instance Ord RootX where
    compare (RootX a b)
            (RootX c d) |  n == 0    = EQ
                        |  n >  0    = GT
                        |  otherwise = LT
                           where n = (a - c) + (((b - d) % 1) * rtX)

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

{-# INLINE addRtX #-}
addRtX               :: RootX -> RootX
addRtX (RootX r1 r2) =  RootX r1 (r2 + 1)

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