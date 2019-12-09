module IntCodeInterpreterTest where

import IntCodeInterpreter
    ( Operation(..)
    , ParameterMode(..)
    , IntCode(..)
    , Mode(..)
    , toOperation
    , initIntCode
    , getValue
    , executeInstruction
    , process
    )

import qualified Data.Map as M

import Test.HUnit


testToOperation =
    [ TestLabel "" $ TestCase $ assertEqual "toInstruction 2" (Mult Position Position Position) (toOperation 2)
    , TestLabel "" $ TestCase $ assertEqual "toInstruction 1002" (Mult Position Immediate Position) (toOperation 1002)
    ]


ic1 = initIntCode [] [2,4,4,5,99,0]
ic2 = initIntCode [] [1,0,0,0,99]
ic3 = initIntCode [8] [3,9,8,9,10,9,4,9,99,-1,8]
ic4 = initIntCode [] [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
ic5 = initIntCode [] [1102,34915192,34915192,7,4,7,99,0]


asMap = M.fromList . zip [0,1..]


testGetValue =
    [ TestLabel "" $ TestCase $ assertEqual "" 99 (getValue Position ic1 1)
    , TestLabel "" $ TestCase $ assertEqual "" 4 (getValue Immediate ic1 2)
    , TestLabel "" $ TestCase $ assertEqual "" 0 (getValue Position ic1 3)
    ]



testExecuteInstruction =
    [ TestLabel "" $ TestCase $ assertEqual ""
        (ic1 { _map = asMap [2,4,4,5,99,9801], currentIndex = 4})
        (executeInstruction (Mult Position Position Position) ic1)
    , TestLabel "" $ TestCase $ assertEqual ""
        (ic2 {_map = asMap [2,0,0,0,99], currentIndex = 4})
        (executeInstruction (Add Position Position Position) ic2)
    , TestLabel "" $ TestCase $ assertEqual ""
        (ic3 {_map = asMap [3,9,8,9,10,9,4,9,99,8,8], currentIndex = 2, inputStrip = []})
        (executeInstruction (ReadFromInput Position) ic3)
    , TestLabel "" $ TestCase $ assertEqual ""
        ic4
            { _map = M.fromList $ (zip [0..] [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]) ++ [(100,2),(101,0)]
            , relativeBase = 2
            , currentIndex = 0
            , outputStrip = [1,109]
            }
        ( executeInstruction (JumpIfFalse Position Immediate)
        $ executeInstruction (Equals Position Immediate Position)
        $ executeInstruction (Add Position Immediate Position)
        $ executeInstruction (WriteToOutput Relative)
        $ executeInstruction (AdjustRelativeBase Immediate)
        $ executeInstruction (JumpIfFalse Position Immediate)
        $ executeInstruction (Equals Position Immediate Position)
        $ executeInstruction (Add Position Immediate Position)
        $ executeInstruction (WriteToOutput Relative)
        $ executeInstruction (AdjustRelativeBase Immediate)
        $ ic4
        )
    ]


testProcess =
    [ TestLabel "" $ TestCase $ assertEqual ""
        ic4
            { _map = M.fromList $ (zip [0..] [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]) ++ [(100,16),(101,1)]
            , relativeBase = 16
            , currentIndex = 16
            , outputStrip = reverse [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
            , mode = Halted
            }
        (process ic4)
    , TestLabel "" $ TestCase $ assertEqual ""
        ic5 {_map = asMap [1102,34915192,34915192,7,4,7,99,1219070632396864], outputStrip = [1219070632396864], currentIndex = 7, mode = Halted}
        (process ic5)
    , TestLabel "" $ TestCase $ assertEqual ""
        ic5 {_map = asMap [104,1125899906842624,99], outputStrip = [1125899906842624], currentIndex = 3, mode = Halted}
        (process $ initIntCode [] [104,1125899906842624,99])
    ]
    

intCodeTests :: [Test]
intCodeTests = testToOperation ++ testGetValue ++ testExecuteInstruction ++ testProcess
