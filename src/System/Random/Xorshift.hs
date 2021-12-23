module System.Random.Xorshift where

import           Data.Word                  ( Word32 )
import           Data.Function              ( (&) )
import           Data.Bits                  ( shiftL
                                            , shiftR
                                            , xor )

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

w32XorshiftLast     :: Word32 -> Word32 -> Word32
w32XorshiftLast n s =  f' n s 0
                       where f' 0 _ x = x
                             f' n s _ =  f' (n-1) ns ns
                                            where ns = xorshiftW32' s
