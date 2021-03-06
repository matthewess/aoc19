{-# LANGUAGE RecordWildCards #-}

module IntCodeInterpreter where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (null)
import Control.Arrow ((&&&))
import Data.Maybe (listToMaybe, fromMaybe)


data ParameterMode = Position | Immediate | Relative deriving (Show, Eq)


toParameterMode :: Char -> ParameterMode
toParameterMode '0' = Position
toParameterMode '1' = Immediate
toParameterMode '2' = Relative
toParameterMode _   = error "Incorrect Parameter ExecutionState"


data Operation
    = Add ParameterMode ParameterMode ParameterMode
    | Mult ParameterMode ParameterMode ParameterMode
    | ReadFromInput ParameterMode
    | WriteToOutput ParameterMode
    | JumpIfTrue ParameterMode ParameterMode
    | JumpIfFalse ParameterMode ParameterMode
    | LessThan ParameterMode ParameterMode ParameterMode
    | Equals ParameterMode ParameterMode ParameterMode
    | AdjustRelativeBase ParameterMode
    | Halt
    deriving (Show, Eq)


toOperation :: Int -> Operation
toOperation i =
    let
        oc = i `mod` 100
        parameterExecutionStates = show $ i `div` 100
        padedParameterModes = reverse $ "0000" ++ parameterExecutionStates
        pms = map toParameterMode padedParameterModes
        pm1 = pms !! 0
        pm2 = pms !! 1
        pm3 = pms !! 2
    in
        case oc of
            1 -> Add pm1 pm2 pm3
            2 -> Mult pm1 pm2 pm3
            3 -> ReadFromInput pm1
            4 -> WriteToOutput pm1
            5 -> JumpIfTrue pm1 pm2
            6 -> JumpIfFalse pm1 pm2
            7 -> LessThan pm1 pm2 pm3
            8 -> Equals pm1 pm2 pm3
            9 -> AdjustRelativeBase pm1
            99 -> Halt


data ExecutionState = Processing | WaitingForInput | Halted deriving (Show, Eq)

data IntCode = IntCode
    { memoryStrip :: M.Map Int Int
    , instructionPointer :: Int
    , inputStrip :: [Int]
    , outputStrip :: [Int]
    , executionState :: ExecutionState
    , relativeBase :: Int
    } deriving (Show, Eq)


(!!!) :: IntCode -> Int -> Int
ic !!! idx = memoryStrip ic M.! idx


getValue :: ParameterMode -> IntCode -> Int -> Int
getValue Position ic@IntCode{..} idx = fromMaybe 0 $ memoryStrip M.!? (memoryStrip M.! idx)
getValue Relative ic@IntCode{..} idx = fromMaybe 0 $ memoryStrip M.!? ((memoryStrip M.! idx) + relativeBase)
getValue Immediate ic@IntCode{..} idx = memoryStrip M.! idx


getAddress :: ParameterMode -> IntCode -> Int -> Int
getAddress Position ic@IntCode{..} idx = memoryStrip M.! idx
getAddress Relative ic@IntCode{..} idx = relativeBase + memoryStrip M.! idx


values :: IntCode -> [Int]
values = M.elems . memoryStrip


executeInstruction :: Operation -> IntCode -> IntCode
executeInstruction (Add pm1 pm2 pm3) ic@IntCode{..} = ic
    { memoryStrip = M.alter (\_ -> Just (p1 + p2)) ri memoryStrip
    , instructionPointer = instructionPointer + 4
    , executionState = Processing
    } where
        p1 = getValue pm1 ic (instructionPointer + 1)
        p2 = getValue pm2 ic (instructionPointer + 2)
        ri = getAddress pm3 ic (instructionPointer + 3)
executeInstruction (Mult pm1 pm2 pm3) ic@IntCode{..} = ic
    { memoryStrip = M.alter (\_ -> Just (p1 * p2)) ri memoryStrip
    , instructionPointer = instructionPointer + 4
    , executionState = Processing
    } where
        p1 = getValue pm1 ic (instructionPointer + 1)
        p2 = getValue pm2 ic (instructionPointer + 2)
        ri = getAddress pm3 ic (instructionPointer + 3)
executeInstruction (ReadFromInput pm1) ic@IntCode{..} =
    if null inputStrip then
        ic {executionState = WaitingForInput}
    else
        ic
            { memoryStrip = M.alter (\_ -> Just (head $ inputStrip)) ri memoryStrip
            , instructionPointer = instructionPointer + 2
            , inputStrip = tail inputStrip
            , executionState = Processing
            }
    where ri = getAddress pm1 ic (instructionPointer + 1)
executeInstruction (WriteToOutput pm1) ic@IntCode{..} = ic
    { instructionPointer = instructionPointer + 2
    , outputStrip = p1 : outputStrip
    , executionState = Processing
    } where
        p1 = getValue pm1 ic (instructionPointer + 1)
executeInstruction (JumpIfTrue pm1 pm2) ic@IntCode{..} = ic
    { instructionPointer = if p1 /= 0 then p2 else (instructionPointer + 3)
    , executionState = Processing
    } where
        p1 = getValue pm1 ic (instructionPointer + 1)
        p2 = getValue pm2 ic (instructionPointer + 2)
executeInstruction (JumpIfFalse pm1 pm2) ic@IntCode{..} = ic
    { instructionPointer = if p1 == 0 then p2 else (instructionPointer + 3)
    , executionState = Processing
    } where
        p1 = getValue pm1 ic (instructionPointer + 1)
        p2 = getValue pm2 ic (instructionPointer + 2)
executeInstruction (LessThan pm1 pm2 pm3) ic@IntCode{..} = ic
    { memoryStrip = M.alter (\_ -> Just (if p1 < p2 then 1 else 0)) ri memoryStrip
    , instructionPointer = instructionPointer + 4
    , executionState = Processing
    } where
        p1 = getValue pm1 ic (instructionPointer + 1)
        p2 = getValue pm2 ic (instructionPointer + 2)
        ri = getAddress pm3 ic (instructionPointer + 3)
executeInstruction (Equals pm1 pm2 pm3) ic@IntCode{..} = ic
    { memoryStrip = M.alter (\_ -> Just (if p1 == p2 then 1 else 0)) ri memoryStrip
    , instructionPointer = instructionPointer + 4
    , executionState = Processing
    } where
        p1 = getValue pm1 ic (instructionPointer + 1)
        p2 = getValue pm2 ic (instructionPointer + 2)
        ri = getAddress pm3 ic (instructionPointer + 3)
executeInstruction Halt ic@IntCode{..} = ic
    { executionState = Halted
    , instructionPointer = instructionPointer + 1
    }
executeInstruction (AdjustRelativeBase pm1) ic@IntCode{..} = ic
    { relativeBase = relativeBase + adjustmentValue
    , instructionPointer = instructionPointer + 2
    } where adjustmentValue = getValue pm1 ic (instructionPointer + 1)


initIntCode :: [Int] -> [Int] -> IntCode
initIntCode inp icStrip = IntCode
    { memoryStrip = M.fromList $ zip [0,1..] icStrip
    , instructionPointer = 0
    , inputStrip = inp
    , outputStrip = []
    , executionState = Processing
    , relativeBase = 0
    }


-- The IntCode Program executor
process :: IntCode -> IntCode
process intMap =
    let
        idx =  instructionPointer intMap
        operation = toOperation (intMap !!! idx)
        ic = executeInstruction operation intMap
    in
        case executionState ic of 
            Halted -> ic
            WaitingForInput -> ic
            otherwise -> process $ ic


appendToInput :: IntCode -> [Int] -> IntCode
appendToInput ic@IntCode{..} i = ic {inputStrip = inputStrip ++ i}


flushOutput :: IntCode -> IntCode
flushOutput ic@IntCode{..} = ic {outputStrip = []}
