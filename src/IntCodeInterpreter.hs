module IntCodeInterpreter where


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


-- The IntCode Program executor
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


-- Interface to the outside world
processWithICI :: [Integer] -> [Integer]
processWithICI = values . process . initIntCode
