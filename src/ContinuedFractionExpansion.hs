module ContinuedFractionExpansion where

import Data.Ratio

cfe          :: [Integer] -> Rational
cfe [x]      =  x % 1
cfe (x : xs) =  (x % 1) + (recipRatio (cfe xs))

recipRatio :: Rational -> Rational
recipRatio r = d % n
               where d = denominator r
                     n = numerator   r
