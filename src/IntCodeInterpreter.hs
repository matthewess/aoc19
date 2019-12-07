{-# LANGUAGE RecordWildCards #-}

module IntCodeInterpreter where

import Data.Map hiding (map)
import Data.List.Split (splitOn)
import Control.Arrow ((&&&))


-- Makeshift Mutable array
data ParameterMode = Position | Immediate deriving (Show, Eq)


toParameterMode :: Char -> ParameterMode
toParameterMode '0' = Position
toParameterMode '1' = Immediate
toParameterMode _   = error "Incorrect Parameter Mode"


data Operation
    = Add ParameterMode ParameterMode
    | Mult ParameterMode ParameterMode
    | ReadFromInput
    | WriteToOutput
    | JumpIfTrue ParameterMode ParameterMode
    | JumpIfFalse ParameterMode ParameterMode
    | LessThan ParameterMode ParameterMode
    | Equals ParameterMode ParameterMode
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
            3 -> ReadFromInput
            4 -> WriteToOutput
            5 -> JumpIfTrue (pms !! 0) (pms !! 1)
            6 -> JumpIfFalse (pms !! 0) (pms !! 1)
            7 -> LessThan (pms !! 0) (pms !! 1)
            8 -> Equals (pms !! 0) (pms !! 1)
            99 -> Halt


data Instruction
    = IAdd Int Int Int
    | IMult Int Int Int
    | IInput Int
    | IOutput Int
    | IJumpIfTrue Int Int
    | IJumpIfFalse Int Int
    | ILessThan Int Int Int
    | IEquals Int Int Int
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
getValue Immediate ic idx = ic !!! idx


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
    in
        case operation of 
            Add pm1 pm2 ->
                IAdd (getValue pm1 ic (idx+1)) (getValue pm2 ic (idx+2)) (ic !!! (idx + 3))
            Mult pm1 pm2 ->
                IMult (getValue pm1 ic (idx+1)) (getValue pm2 ic (idx+2)) (ic !!! (idx + 3))
            ReadFromInput ->
                IInput (ic !!! (idx + 1))
            WriteToOutput ->
                IOutput (ic !!! (idx + 1))
            JumpIfTrue pm1 pm2 ->
                IJumpIfTrue (getValue pm1 ic (idx+1)) (getValue pm2 ic (idx+2))
            JumpIfFalse pm1 pm2 ->
                IJumpIfFalse (getValue pm1 ic (idx+1)) (getValue pm2 ic (idx+2))
            LessThan pm1 pm2 ->
                ILessThan (getValue pm1 ic (idx+1)) (getValue pm2 ic (idx+2)) (ic !!! (idx + 3))
            Equals pm1 pm2 ->
                IEquals (getValue pm1 ic (idx+1)) (getValue pm2 ic (idx+2)) (ic !!! (idx + 3))
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
executeInstruction (IInput ri) IntCode{..} = IntCode
    { _map = update (\_ -> Just (head inputStrip)) ri _map
    , currentIndex = currentIndex + 2
    , inputStrip = tail inputStrip
    , outputStrip = outputStrip
    }
executeInstruction (IOutput ri) IntCode{..} = IntCode
    { _map = _map
    , currentIndex = currentIndex + 2
    , inputStrip = inputStrip
    , outputStrip = (_map ! ri) : outputStrip
    }
executeInstruction (IJumpIfTrue p1 p2) IntCode{..} = IntCode
    { _map = _map
    , currentIndex = if p1 /= 0 then p2 else (currentIndex + 3)
    , inputStrip = inputStrip
    , outputStrip = outputStrip
    }
executeInstruction (IJumpIfFalse p1 p2) IntCode{..} = IntCode
    { _map = _map
    , currentIndex = if p1 == 0 then p2 else (currentIndex + 3)
    , inputStrip = inputStrip
    , outputStrip = outputStrip
    }
executeInstruction (ILessThan p1 p2 ri) IntCode{..} = IntCode
    { _map = update (\_ -> Just (if p1 < p2 then 1 else 0)) ri _map
    , currentIndex = currentIndex + 4
    , inputStrip = inputStrip
    , outputStrip = outputStrip
    }
executeInstruction (IEquals p1 p2 ri) IntCode{..} = IntCode
    { _map = update (\_ -> Just (if p1 == p2 then 1 else 0)) ri _map
    , currentIndex = currentIndex + 4
    , inputStrip = inputStrip
    , outputStrip = outputStrip
    }
executeInstruction IHalt ic = error "This method should not be called on Halt"



initIntCode :: [Int] -> [Int] -> IntCode
initIntCode inp icStrip = IntCode
    { _map = fromList $ zip [0,1..] icStrip
    , currentIndex = 0
    , inputStrip = inp
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
processWithICI :: [Int] -> [Int] -> [Int]
processWithICI ys = values . process . initIntCode ys


processOutputWithICI :: [Int] -> [Int] -> [Int]
processOutputWithICI ys = outputStrip . process . initIntCode ys
