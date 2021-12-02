{-# LANGUAGE TemplateHaskell #-}

module Data.ErgodicPRNG where

import Data.Root
import Data.Ratio
import Control.Lens ( (^.)
                    , makeLenses )

data RState = RState { _x     :: Root
                     , _y     :: Root
                     , _bx    :: Bool
                     , _by    :: Bool
                     , _l     :: Rational
                     , _count :: Integer
                     } deriving ( Show )

makeLenses ''RState

mkRst         :: Rational -> Root -> Root -> RState
mkRst l sx sy =  RState { _x     = sx 
                        , _y     = sy
                        , _bx    = True
                        , _by    = True
                        , _l     = l
                        , _count = 0 }


s1 = (1 % 2) -/ 1
s2 = (1 % 2) -/ 1

lx :: Root
lx =  gr
ly :: Root
ly =  1

re = ((3 % 2) -/1) + ((-1 % 2) -/5) -- 1/2.618

f         :: Rational
          -> Root
          -> Root
          -> RState
f l sx sy =  go (mkRst l sx sy)

makecsv              :: Rational -> Integer -> Root -> Root -> IO ()
makecsv l n sx sy =  do putStrLn  (randomstr l n sx sy)
                        writeFile ("X_"   ++ show (toFloating sx) ++
                                  "_Y_"   ++ show (toFloating sy) ++
                                  "_Len_" ++ show (numerator l)   ++
                                  "-"     ++ show (denominator l) ++
                                  "_n_"   ++ show n ++ ".csv")
                                  (randomstr l n sx sy)

appendCSV l n sx sy =  do appendFile fn "n, X, Y\n"
                          f' n (mkRst l sx sy) fn
                          where fn = ("X_"   ++ show (toFloating sx) ++
                                     "_Y_"   ++ show (toFloating sy) ++
                                     "_Len_" ++ show (numerator l)   ++
                                     "-"     ++ show (denominator l) ++
                                     "_n_"   ++ show n ++ ".csv")

f' n rst fn |  rst ^. count == n = return ()
            |  otherwise         = do putStrLn $ putRst rst
                                      appendFile fn $ putRst rst
                                      f' n (go rst) fn

putRst     :: RState -> String
putRst rst =  show (rst ^. count)          ++ ", " ++
              show (toFloating $ rst ^. x) ++ ", " ++
              show (toFloating $ rst ^. y) ++ "\n"

randomstr           :: Rational -> Integer -> Root -> Root -> String
randomstr l n sx sy =  "n, X, Y\n0, " ++
                       show (toFloating sx) ++ ", " ++ 
                       show (toFloating sy) ++ "\n" ++
                       randoms''' n (mkRst l sx sy)

randoms'''       :: Integer -> RState -> String
randoms''' n rst |  rst ^. count == n = ""
                 |  otherwise         = show (rst ^. count)           ++ ", " ++
                                        show (toFloating $ nrst ^. x) ++ ", " ++
                                        show (toFloating $ nrst ^. y) ++ "\n" ++
                                        randoms''' n nrst
                                        where nrst = go rst

randoms         :: Floating a => Rational -> Root -> Root -> [a]
randoms l sx sy =  toFloating ((sx + sy) * re) : randoms'' (mkRst l sx sy)

randoms'' rst =  toFloating ((nrst ^. x + nrst ^. y) * re) : randoms'' nrst
                 where nrst = go rst

randoms' rst =  (toFloating $ nrst ^. x, toFloating $ nrst ^. y) : randoms' nrst
                where nrst = go rst

go     :: RState -> RState
go rst =  rst { _x     = nx
              , _y     = ny
              , _bx    = nbx
              , _by    = nby
              , _count = rst ^. count + 1 }
          where ln  = ((rst ^. l) * (1 % 2)) -/ 2
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
                          {-
                (ny, nby) | modRoot ((rst ^. y) + ln)
                                    (ly * 2)
                            < 1       = (((rst ^. y) + ln) `modRoot` 2,       True)
                          | otherwise = (2 - (((rst ^. y) + ln) `modRoot` 2), False)
                          -}
