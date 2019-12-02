module D021202ProgramAlarm where

import Data.Map hiding (map)
import Prelude hiding ((!!))
import Data.List.Split (splitOn)
import Control.Arrow ((&&&))


-- Makeshift Mutable array
data IntCode = IntCode
    { _map :: Map Integer Integer
    , currentIndex :: Integer
    }


initIntCode :: [Integer] -> IntCode
initIntCode xs = IntCode
    { _map = fromList $ zip [0,1..] xs
    , currentIndex = 0
    }


(!!) :: IntCode -> Integer -> Integer
ic !! idx = _map ic ! idx


values :: IntCode -> [Integer]
values = elems . _map


updateIntMap :: IntCode -> Integer -> Integer -> Integer -> IntCode
updateIntMap intMap resultIndex result currentIdx = IntCode
    { _map = update (\_ -> Just result) resultIndex $ _map intMap
    , currentIndex = currentIdx
    }


-- Part I

process :: IntCode -> IntCode
process intMap
    | idxVal == 1 = process add
    | idxVal == 2 = process mult
    | idxVal == 99 = intMap
    | otherwise = error $ "Error in Input" ++ show idxVal
    where
        idx =  currentIndex intMap
        idxVal = intMap !! idx
        indices = (idx + 1, idx + 2, idx + 3)
        firstNumber = intMap !! (intMap !! (idx + 1))
        secondNumber = intMap !! (intMap !! (idx + 2))
        resultIndex = intMap !! (idx+3)
        updatedIndex = idx + 4
        add = updateIntMap intMap resultIndex (firstNumber + secondNumber) updatedIndex
        mult = updateIntMap intMap resultIndex (firstNumber * secondNumber) updatedIndex


intCode :: [Integer] -> [Integer]
intCode = values . process . initIntCode

replaceOldState :: Integer -> Integer -> [Integer] -> [Integer]
replaceOldState noun verb (x0:_:_:xs) = x0 : noun : verb : xs
replaceOldState _ _ _ = error "Incorrect input"


p1 :: [Integer] -> Integer
p1 = head . intCode . replaceOldState 12 2


-- Part II

p2 :: [Integer] -> Integer
p2 xs = 
    let
        f noun verb = head . intCode . replaceOldState noun verb $ xs
        allNounVerbPairs =  (,) <$> [0,1..99] <*> [0,1..99]
        combine (noun, verb) = 100 * noun + verb
    in
        combine . head . dropWhile (\(noun, verb) -> f noun verb /= 19690720) $ allNounVerbPairs


-- Runner

main :: IO ()
main = getContents >>=
    print . (p1 &&& p2) . map read . splitOn ","
