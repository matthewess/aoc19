module D05SunnyWithAChanceOfAsteroids where

import Data.List.Split (splitOn)
import Control.Arrow ((&&&))
import IntCodeInterpreter (processOutputWithICI)


p1 :: [Int] -> [Int]
p1 = processOutputWithICI [1]

p2 :: [Int] -> Int
p2 _ = 2


main :: IO ()
main = getContents >>=
    print . (p1 &&& p2) . map read . splitOn ","
