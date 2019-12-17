--+
import BasePrelude

import Base
import Reps
import Data
import System.IO

decimalAlign :: Int -> String -> String
''  n s = replicate (n-k) ' ' ++ s where
   k = fromMaybe (length s) $ elemIndex '.' s

main = do
   let pops = census2010
   let format (a, b) = decimalAlign 9 (show a) ++ decimalAlign 9 b
   let llist = fromList [ boundLeastError n pops app
                  | (n, app) <- zip [1..] $ tail $ toList
                     $ apportionStream websterComparerPref1 pops ]
   hSetBuffering stdout LineBuffering
   -- print an infinite stream
   mapM_ (putStrLn . format) do
      (x :: Int, BoundValue { value = v }) <- zip [1..] $ toList llist
      pure $ (x,) $ showApprox 6 $ uniqueApprox 6 llist v

