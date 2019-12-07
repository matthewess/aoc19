{-# LANGUAGE RecordWildCards #-}

module IntCodeInterpreter where

import Data.Map hiding (map)
import Data.List.Split (splitOn)
import Control.Arrow ((&&&))


-- Makeshift Mutable array
data ParameterMode = Position deriving (Show, Eq)


toParameterMode :: Char -> ParameterMode
toParameterMode '0' = Position
toParameterMode _   = error "Incorrect Parameter Mode"


data Operation
    = Add ParameterMode ParameterMode
    | Mult ParameterMode ParameterMode
    | Halt
    deriving (Show, Eq)


toOperation :: Int -> Operation
toOperation i =
    let
        oc = i `mod` 100
        parameterModes = show $ i `div` 100
        padedParameterModes = reverse $ "0000" ++ parameterModes
        pms = map toParameterMode padedParameterModes
    in
        case oc of
            1  -> Add (pms !! 0) (pms !! 1)
            2  -> Mult (pms !! 0) (pms !! 1)
            99 -> Halt


data Instruction
    = IAdd Int Int Int
    | IMult Int Int Int
    | IHalt
    deriving (Show, Eq)


data IntCode = IntCode
    { _map :: Map Int Int
    , currentIndex :: Int
    , inputStrip :: [Int]
    , outputStrip :: [Int]
    }


(!!!) :: IntCode -> Int -> Int
ic !!! idx = _map ic ! idx


getValue :: ParameterMode -> IntCode -> Int -> Int
getValue Position ic idx = ic !!! (ic !!! idx)


values :: IntCode -> [Int]
values = elems . _map


updateIntMap :: IntCode -> Int -> Int -> Int -> IntCode
updateIntMap IntCode{..} resultIndex result currentIdx = IntCode
    { _map = update (\_ -> Just result) resultIndex $ _map
    , currentIndex = currentIdx
    , inputStrip = inputStrip
    , outputStrip = outputStrip
    }


getInstruction :: Operation -> IntCode -> Instruction
getInstruction o ic =
    let
        idx =  currentIndex ic
        operation = toOperation (ic !!! idx)
        firstNumber = getValue Position ic (idx + 1)
        secondNumber = getValue Position ic (idx + 2)
        resultIndex = ic !!! (idx + 3)
    in
        case operation of 
            Add pm1 pm2 ->
                IAdd firstNumber secondNumber resultIndex
            Mult pm1 pm2 ->
                IMult firstNumber secondNumber resultIndex
            Halt ->
                IHalt


executeInstruction :: Instruction -> IntCode -> IntCode
executeInstruction (IAdd p1 p2 ri) IntCode{..} = IntCode
    { _map = update (\_ -> Just (p1 + p2)) ri _map
    , currentIndex = currentIndex + 4
    , inputStrip = inputStrip
    , outputStrip = outputStrip
    }
executeInstruction (IMult p1 p2 ri) IntCode{..} = IntCode
    { _map = update (\_ -> Just (p1 * p2)) ri _map
    , currentIndex = currentIndex + 4
    , inputStrip = inputStrip
    , outputStrip = outputStrip
    }
executeInstruction IHalt ic = error "This method should not be called on Halt"



initIntCode :: [Int] -> IntCode
initIntCode xs = IntCode
    { _map = fromList $ zip [0,1..] xs
    , currentIndex = 0
    , inputStrip = [1] -- hard coded
    , outputStrip = []
    }


-- The IntCode Program executor
process :: IntCode -> IntCode
process intMap =
    let
        idx =  currentIndex intMap
        operation = toOperation (intMap !!! idx)
        instruction = getInstruction operation intMap
    in
        case instruction of 
            IHalt ->
                intMap
            i ->
                process $ executeInstruction i intMap
        

-- Interface to the outside world
processWithICI :: [Int] -> [Int]
processWithICI = values . process . initIntCode
