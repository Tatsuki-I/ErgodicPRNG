module Data.Irrational where

import           Data.Ratio ( (%)
                            , numerator
                            , denominator )
import           GHC.Real   ( Ratio ( (:%) ) )
import           ContinuedFractionExpansion ( cfe )

data Irrational = Irrational Rational
                             Rational -- Rational Term of Irrational number
                             deriving ( Eq )

instance Show Irrational where
    {-# INLINE show #-}
    show (Irrational 0 0) = "0"
    show (Irrational 0 b) = show b ++ " * phi"
    show (Irrational a 0) = show a
    show (Irrational a b) = show a ++ " + " ++ show b ++ " * phi"

instance Num Irrational where
    {-# INLINE (+) #-}
    (+) (Irrational a b)
        (Irrational c d) = Irrational (a + c) (b + d)

    {-# INLINE signum #-}
    signum r | r' > 0    =   1
             | r' < 0    = - 1
             | otherwise =   0
               where r'  = toRationalIr r

    {-# INLINE negate #-}
    negate (Irrational a b) = Irrational (-a) (-b)

    {-# INLINE abs #-}
    abs r = r * signum r

    {-# INLINE fromInteger #-}
    fromInteger a = Irrational (a % 1) 0

{-# INLINE toFloatingIr #-}
toFloatingIr :: Floating a => Irrational -> a
toFloatingIr =  fromRational . toRationalIr

{-# INLINE toRationalIr #-}
toRationalIr             :: Irrational -> Rational
toRationalIr (Irrational a b) =  a + (toRational b) * phi

instance Ord Irrational where
    compare (Irrational a b)
            (Irrational c d) |  n == 0    = EQ
                        |  n >  0    = GT
                        |  otherwise = LT
                           where n = (a - c) + ((b - d) * phi)

{-# INLINE divIr #-}
divIr     :: Irrational -> Irrational -> Irrational
divIr a b |  a >= b = 1 + divIr (a - b) b
          |  a <  b = 0

{-# INLINE modIr #-}
modIr     :: Irrational -> Irrational -> Irrational
modIr a b |  a == b = Irrational 0 0
          |  a >  b = modIr (a - b) b
          |  a <  b = a

{-# INLINE sub1 #-}
sub1               :: Irrational -> Irrational
sub1 (Irrational r1 r2) =  Irrational (r1 - 1) r2

{-# INLINE addIr #-}
addIr               :: Irrational -> Irrational
addIr (Irrational r1 r2) =  Irrational r1 (r2 + 1)

{-# INLINE cpIr1 #-}
cpIr1           :: Irrational -> Ordering
cpIr1 (Irrational 1 _)                       = EQ
cpIr1 r                |  toRationalIr r > 1 = GT
                       |  otherwise          = LT

{-# INLINE modIr1 #-}
modIr1   :: Irrational -> Irrational
modIr1 a =  case cpIr1 a of
                 GT -> modIr1 (sub1 a)
                 LT -> a
                 EQ -> Irrational 0 0

--evenIr   :: Irrational -> Bool
--evenIr r =  (r `modIr` 2) == 0

evenIr r =  even (n `div` d)
             where n :% d = toRationalIr r

oddIr   :: Irrational -> Bool
oddIr r =  not (evenIr r)

{-# INLINE rt5 #-}
rt5 :: Rational
-- rt5 =  2.236067977499789696 -- -/5
rt5 = 5374978561 % 2403763488 -- Continued Fraction(n=15)

{-# INLINE rt2 #-}
rt2 :: Rational
-- rt2 =  1.414213562373095048
rt2 = 4478554083 % 3166815962 -- Continued Fraction(n=25)

crt4 :: Rational
crt4 =  1.5874010519

piRat :: Rational
piRat =  1285290289249 % 409120605684

crt12 :: Rational
crt12 = cfe [2, 3, 2, 5, 15, 7, 3, 1, 1, 3, 1, 1, 96]--, 7, 2, 6, 3, 36, 1, 17, 25]

{-# INLINE phi #-}
phi :: Rational
phi =  crt12
--ir =  2.2
