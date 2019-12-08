module IntCodeInterpreterTest where

import IntCodeInterpreter
    ( Operation(..)
    , ParameterMode(..)
    , Instruction(..)
    , IntCode(..)
    , toOperation
    , initIntCode
    , getValue
    , getInstruction
    , executeInstruction
    )

import qualified Data.Map as M

import Test.HUnit


testToOperation =
    [ TestLabel "" $ TestCase $ assertEqual "toInstruction 2" (Mult Position Position) (toOperation 2)
    , TestLabel "" $ TestCase $ assertEqual "toInstruction 1002" (Mult Position Immediate) (toOperation 1002)
    ]


ic1 = initIntCode [] [2,4,4,5,99,0]
ic2 = initIntCode [] [1,0,0,0,99]
ic3 = initIntCode [8] [3,9,8,9,10,9,4,9,99,-1,8]


asMap = M.fromList . zip [0,1..]


testGetValue =
    [ TestLabel "" $ TestCase $ assertEqual "" 99 (getValue Position ic1 1)
    , TestLabel "" $ TestCase $ assertEqual "" 4 (getValue Immediate ic1 2)
    , TestLabel "" $ TestCase $ assertEqual "" 0 (getValue Position ic1 3)
    ]


testGetInstruction =
    [ TestLabel "" $ TestCase $ assertEqual "" (IMult 99 99 5) (getInstruction (Mult Position Position) ic1)
    , TestLabel "" $ TestCase $ assertEqual "" (IAdd 99 99 5) (getInstruction (Add Position Position) ic1)
    , TestLabel "" $ TestCase $ assertEqual "" (IHalt) (getInstruction Halt ic1)
    ]


testExecuteInstruction =
    [ TestLabel "" $ TestCase $ assertEqual "" (ic1 { _map = asMap [2,4,4,5,99,9801], currentIndex = 4}) (executeInstruction (IMult 99 99 5) ic1)
    , TestLabel "" $ TestCase $ assertEqual "" (ic2 {_map = asMap [2,0,0,0,99], currentIndex = 4}) (executeInstruction (IAdd 1 1 0) ic2)
    , TestLabel "" $ TestCase $ assertEqual "" (ic3 {_map = asMap [3,9,8,9,10,9,4,9,99,8,8], currentIndex = 2, inputStrip = []}) (executeInstruction (IInput 9) ic3)
    ]
    

intCodeTests :: [Test]
intCodeTests = testToOperation ++ testGetValue ++ testExecuteInstruction
