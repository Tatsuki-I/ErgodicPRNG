module System.Random.RDRAND where

import           Data.Word                  ( Word8
                                            , Word32 )
import qualified Data.ByteString.Internal   as BI
import           System.Entropy             ( getHardwareEntropy
                                            , getEntropy )
import           Data.Bits                  ( shiftL
                                            , (.|.) )

fastRandom nr = do s <- maybe (getEntropy nr) pure =<< getHardwareEntropy (nr * 4)
                   print $ w8ToW32 $ BI.unpackBytes s

w8ToW32                          :: [Word8] -> [Word32]
w8ToW32 []                       =  []
w8ToW32 (w1 : w2 : w3 : w4 : ws) =  w8ToW32' (w1, w2, w3, w4) : w8ToW32 ws

fastRandomSum nr = do s <- maybe (undefined) pure =<< getHardwareEntropy (nr * 4)
                      print $ w8ToW32Sum (BI.unpackBytes s) 0

w8ToW32Sum                            :: [Word8] -> Word32 -> Word32
w8ToW32Sum []                       x =  x
w8ToW32Sum (w1 : w2 : w3 : w4 : ws) _ =  w8ToW32Sum ws (w8ToW32' (w1, w2, w3, w4))

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
