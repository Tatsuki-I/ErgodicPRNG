module RatioNumbers where

rns   :: Int -> IO ()
rns i =  f i (show i ++ ".csv") ""
         where f 0 fn str = appendFile fn str
               f i fn str = f (i - 1) fn (str ++ rn i)

rn   :: Int -> String
rn d =  f 1 d ""
        where f n d str |  n <= d    = f (n + 1) d (str ++ show (fromIntegral n / fromIntegral d) ++ ",0\n")
                        |  otherwise = str
