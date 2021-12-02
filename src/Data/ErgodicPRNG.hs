{-# LANGUAGE TemplateHaskell #-}

module Data.ErgodicPRNG where

import qualified Data.ByteString.Lazy as BS
import           Data.Root
import           Data.Binary     ( encode )
import           Data.Ratio
import           Data.Word       ( Word32 )
import           Control.Lens    ( (^.)
                                 , makeLenses )

data RState = RState { _x     :: Root
                     , _y     :: Root
                     , _bx    :: Bool
                     , _by    :: Bool
                     , _l     :: Root
                     -- , _l     :: Rational
                     , _count :: Integer
                     } deriving ( Show )

makeLenses ''RState

mkRst         :: Root -> Root -> Root -> RState
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

fn n sx sy =  "X_"   ++ show (toFloating sx) ++
              "_Y_"   ++ show (toFloating sy) ++
              "_n_"   ++ show n ++ ".csv"

appendCSV l n sx sy =  do appendFile (fn n sx sy) "n, X, Y\n"
                          f' n (mkRst l sx sy) (fn n sx sy)

f' n rst fn |  rst ^. count == n = return ()
            |  otherwise         = do putStrLn $ putRst rst
                                      appendFile fn $ putRst rst
                                      f' n (go rst) fn

appendCSVyInt l n sx sy s i =  fYInt n (mkRst l sx sy) ("Int" ++ show i ++ "_" ++ fn n sx sy) s i

fYInt n rst fn s i |  rst ^. count == n = return ()
                   |  otherwise         = do putStrLn    (show (rst ^. count) ++ "    " ++ show (mapInt s i $ rst ^. y))
                                             appendFile fn ((show $ mapInt s i $ rst ^. y) ++ "\n")
                                             fYInt n (go rst) fn s i

appendCSVyIntBS l n sx sy =  fYIntBS n (mkRst l sx sy) ("UInt32_n-" ++ show n ++ ".bin") 0

fYIntBS n rst fn c |  rst ^. count == n = return ()
                   |  otherwise         = do print c
                                             BS.appendFile fn (encode cy)
                                             fYIntBS n (go rst) fn (c + 1)
                                             where cy :: Word32
                                                   cy =  mapInt False 32 $ rst ^. y

appendCSVyFloat l n sx sy =  fYFloat n (mkRst l sx sy) ("Float_" ++ fn n sx sy)

fYFloat n rst fn |  rst ^. count == n = return ()
                 |  otherwise         = do putStrLn      (show $ toFloating $ rst ^. y)
                                           appendFile fn ((show $ toFloating $ rst ^. y) ++ "\n")
                                           fYFloat n (go rst) fn

putRst     :: RState -> String
putRst rst =  show (rst ^. count)          ++ ", " ++
              show (toFloating $ rst ^. x) ++ ", " ++
              show (toFloating $ rst ^. y) ++ "\n"

randomstr           :: Root -> Integer -> Root -> Root -> String
randomstr l n sx sy =  "n, X, Y\n0, " ++
                       show (toFloating sx) ++ ", " ++ 
                       show (toFloating sy) ++ "\n" ++
                       randoms''' n (mkRst l sx sy) (toFloating)

mapInt       :: Integral a
             => Bool       -- ^ Signed
             -> Int        -- ^ Bits
             -> Root
             -> a
mapInt s i r =  floor (toFloating (r * (mb s i)))
              where mb         :: Bool -> Int -> Root
                    mb True  8  =  127
                    mb True  16 =  32767
                    mb True  32 =  2147483647
                    mb True  64 =  9223372036854775807
                    mb False 8  =  255
                    mb False 16 =  65535
                    mb False 32 =  4294967295
                    mb False 64 =  18446744073709551615


randoms'''           :: Show a => Integer -> RState -> (Root -> a) -> [Char]
randoms''' n rst (f) |  rst ^. count == n = ""
                     |  otherwise         = show (rst ^. count)  ++ ", " ++
                                            show (f $ nrst ^. x) ++ ", " ++
                                            show (f $ nrst ^. y) ++ "\n" ++
                                            randoms''' n nrst (f)
                                            where nrst = go rst

randoms         :: Floating a => Root -> Root -> Root -> [a]
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
                          {-
                (ny, nby) | modRoot ((rst ^. y) + ln)
                                    (ly * 2)
                            < 1       = (((rst ^. y) + ln) `modRoot` 2,       True)
                          | otherwise = (2 - (((rst ^. y) + ln) `modRoot` 2), False)
                          -}
