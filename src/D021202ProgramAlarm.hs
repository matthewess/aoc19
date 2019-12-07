module D021202ProgramAlarm where

import Data.Map hiding (map)
import Prelude hiding ((!!))
import Data.List.Split (splitOn)
import Control.Arrow ((&&&))
import IntCodeInterpreter (processWithICI)


-- Part I

replaceOldState :: Int -> Int -> [Int] -> [Int]
replaceOldState noun verb (x0:_:_:xs) = x0 : noun : verb : xs
replaceOldState _ _ _ = error "Incorrect input"


p1 :: [Int] -> Int
p1 = head . processWithICI . replaceOldState 12 2


-- Part II

p2 :: [Int] -> Int
p2 xs = 
    let
        f noun verb = head . processWithICI . replaceOldState noun verb $ xs
        allNounVerbPairs =  (,) <$> [0,1..99] <*> [0,1..99]
        combine (noun, verb) = 100 * noun + verb
    in
        combine . head . dropWhile (\(noun, verb) -> f noun verb /= 19690720) $ allNounVerbPairs


-- Runner

main :: IO ()
main = getContents >>=
    print . (p1 &&& p2) . map read . splitOn ","
