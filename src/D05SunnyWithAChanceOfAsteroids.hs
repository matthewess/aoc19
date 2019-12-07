module D05SunnyWithAChanceOfAsteroids where

import Data.List.Split (splitOn)
import Control.Arrow ((&&&))
import IntCodeInterpreter (outputStrip, process, initIntCode)


processOutputWithICI :: [Int] -> [Int] -> [Int]
processOutputWithICI ys = outputStrip . process . initIntCode ys


p1 :: [Int] -> [Int]
p1 = processOutputWithICI [1]

p2 :: [Int] -> [Int]
p2 = processOutputWithICI [5]


main :: IO ()
main = getContents >>=
    print . (p1 &&& p2) . map read . splitOn ","
