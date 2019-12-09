{-# LANGUAGE RecordWildCards #-}

module IntCodeInterpreter where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (null)
import Control.Arrow ((&&&))
import Data.Maybe (listToMaybe)


-- Makeshift Mutable array
data ParameterMode = Position | Immediate deriving (Show, Eq)


toParameterMode :: Char -> ParameterMode
toParameterMode '0' = Position
toParameterMode '1' = Immediate
toParameterMode _   = error "Incorrect Parameter Mode"


data Operation
    = Add ParameterMode ParameterMode ParameterMode
    | Mult ParameterMode ParameterMode ParameterMode
    | ReadFromInput ParameterMode
    | WriteToOutput ParameterMode
    | JumpIfTrue ParameterMode ParameterMode ParameterMode
    | JumpIfFalse ParameterMode ParameterMode ParameterMode
    | LessThan ParameterMode ParameterMode ParameterMode
    | Equals ParameterMode ParameterMode ParameterMode
    | Halt
    deriving (Show, Eq)


toOperation :: Int -> Operation
toOperation i =
    let
        oc = i `mod` 100
        parameterModes = show $ i `div` 100
        padedParameterModes = reverse $ "0000" ++ parameterModes
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
            5 -> JumpIfTrue pm1 pm2 pm3
            6 -> JumpIfFalse pm1 pm2 pm3
            7 -> LessThan pm1 pm2 pm3
            8 -> Equals pm1 pm2 pm3
            99 -> Halt


data Mode = Process | WaitingForInput | Halted deriving (Show, Eq)

data IntCode = IntCode
    { _map :: M.Map Int Int
    , currentIndex :: Int
    , inputStrip :: [Int]
    , outputStrip :: [Int]
    , mode :: Mode
    } deriving (Show, Eq)


(!!!) :: IntCode -> Int -> Int
ic !!! idx = _map ic M.! idx


getValue :: ParameterMode -> IntCode -> Int -> Int
getValue Position ic idx = ic !!! (ic !!! idx)
getValue Immediate ic idx = ic !!! idx


values :: IntCode -> [Int]
values = M.elems . _map


executeInstruction :: Operation -> IntCode -> IntCode
executeInstruction (Add pm1 pm2 pm3) ic@IntCode{..} = ic
    { _map = M.update (\_ -> Just (p1 + p2)) ri _map
    , currentIndex = currentIndex + 4
    , mode = Process
    } where
        p1 = getValue pm1 ic (currentIndex + 1)
        p2 = getValue pm2 ic (currentIndex + 2)
        ri = (ic !!! (currentIndex + 3))
executeInstruction (Mult pm1 pm2 pm3) ic@IntCode{..} = ic
    { _map = M.update (\_ -> Just (p1 * p2)) ri _map
    , currentIndex = currentIndex + 4
    , mode = Process
    } where
        p1 = getValue pm1 ic (currentIndex + 1)
        p2 = getValue pm2 ic (currentIndex + 2)
        ri = (ic !!! (currentIndex + 3))
executeInstruction (ReadFromInput pm1) ic@IntCode{..} =
    if null inputStrip then
        ic {mode = WaitingForInput}
    else
        ic
            { _map = M.update (\_ -> Just (head $ inputStrip)) ri _map
            , currentIndex = currentIndex + 2
            , inputStrip = tail inputStrip
            , mode = Process
            }
    where ri = (ic !!! (currentIndex + 1))
executeInstruction (WriteToOutput pm1) ic@IntCode{..} = ic
    { currentIndex = currentIndex + 2
    , outputStrip = (_map M.! ri) : outputStrip
    , mode = Process
    } where ri = ic !!! (currentIndex + 1)
executeInstruction (JumpIfTrue pm1 pm2 pm3) ic@IntCode{..} = ic
    { currentIndex = if p1 /= 0 then p2 else (currentIndex + 3)
    , mode = Process
    } where
        p1 = getValue pm1 ic (currentIndex + 1)
        p2 = getValue pm2 ic (currentIndex + 2)
executeInstruction (JumpIfFalse pm1 pm2 pm3) ic@IntCode{..} = ic
    { currentIndex = if p1 == 0 then p2 else (currentIndex + 3)
    , mode = Process
    } where
        p1 = getValue pm1 ic (currentIndex + 1)
        p2 = getValue pm2 ic (currentIndex + 2)
executeInstruction (LessThan pm1 pm2 pm3) ic@IntCode{..} = ic
    { _map = M.update (\_ -> Just (if p1 < p2 then 1 else 0)) ri _map
    , currentIndex = currentIndex + 4
    , mode = Process
    } where
        p1 = getValue pm1 ic (currentIndex + 1)
        p2 = getValue pm2 ic (currentIndex + 2)
        ri = (ic !!! (currentIndex + 3))
executeInstruction (Equals pm1 pm2 pm3) ic@IntCode{..} = ic
    { _map = M.update (\_ -> Just (if p1 == p2 then 1 else 0)) ri _map
    , currentIndex = currentIndex + 4
    , mode = Process
    } where
        p1 = getValue pm1 ic (currentIndex + 1)
        p2 = getValue pm2 ic (currentIndex + 2)
        ri = (ic !!! (currentIndex + 3))
executeInstruction Halt ic = ic
    { mode = Halted
    }


initIntCode :: [Int] -> [Int] -> IntCode
initIntCode inp icStrip = IntCode
    { _map = M.fromList $ zip [0,1..] icStrip
    , currentIndex = 0
    , inputStrip = inp
    , outputStrip = []
    , mode = Process
    }


-- The IntCode Program executor
process :: IntCode -> IntCode
process intMap =
    let
        idx =  currentIndex intMap
        operation = toOperation (intMap !!! idx)
        ic = executeInstruction operation intMap
    in
        case mode ic of 
            Halted -> ic
            WaitingForInput -> ic
            otherwise -> process $ ic


appendToInput :: IntCode -> [Int] -> IntCode
appendToInput ic@IntCode{..} i = ic {inputStrip = inputStrip ++ i}


flushOutput :: IntCode -> IntCode
flushOutput ic@IntCode{..} = ic {outputStrip = []}
