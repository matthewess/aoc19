module IntCodeInterpreterTest where

import IntCodeInterpreter
    ( processWithICI
    , Operation(..)
    , ParameterMode(..)
    , toOperation
    , processOutputWithICI
    )

import Test.HUnit


testIntCode =
    [ TestLabel "" $ TestCase $ assertEqual "processWithICI [1,0,0,0,99]" [2,0,0,0,99] (processWithICI [] [1,0,0,0,99])
    , TestLabel "" $ TestCase $ assertEqual "processWithICI [2,3,0,3,99]" [2,3,0,6,99] (processWithICI [] [2,3,0,3,99])
    , TestLabel "" $ TestCase $ assertEqual "processWithICI [2,4,4,5,99,0]" [2,4,4,5,99,9801] (processWithICI [] [2,4,4,5,99,0])
    , TestLabel "" $ TestCase $ assertEqual "processWithICI [1,1,1,4,99,5,6,0,99]" [30,1,1,4,2,5,6,0,99] (processWithICI [] [1,1,1,4,99,5,6,0,99])
    , TestLabel "" $ TestCase $ assertEqual "processWithICI [3,0,4,0,99]" [1,0,4,0,99] (processWithICI [1] [3,0,4,0,99])
    , TestLabel "" $ TestCase $ assertEqual "processWithICI [1101,100,-1,4,0]" [1101,100,-1,4,99] (processWithICI [] [1101,100,-1,4,0])
    ]

testToOperation =
    [ TestLabel "" $ TestCase $ assertEqual "toInstruction 1002" (Mult Position Position) (toOperation 2)
    ]

testOutputFromIntCode =
    [ TestLabel "" $ TestCase $ assertEqual "processWithICI [3,9,8,9,10,9,4,9,99,-1,8]" [1] (processOutputWithICI [8] [3,9,8,9,10,9,4,9,99,-1,8])
    , TestLabel "" $ TestCase $ assertEqual "processWithICI [3,9,8,9,10,9,4,9,99,-1,8]" [0] (processOutputWithICI [9] [3,9,8,9,10,9,4,9,99,-1,8])
    , TestLabel "" $ TestCase $ assertEqual "processWithICI [3,9,7,9,10,9,4,9,99,-1,8]" [1] (processOutputWithICI [7] [3,9,7,9,10,9,4,9,99,-1,8])
    , TestLabel "" $ TestCase $ assertEqual "processWithICI [3,9,7,9,10,9,4,9,99,-1,8]" [0] (processOutputWithICI [9] [3,9,7,9,10,9,4,9,99,-1,8])
    , TestLabel "" $ TestCase $ assertEqual "processWithICI [3,3,1108,-1,8,3,4,3,99]" [1] (processOutputWithICI [8] [3,3,1108,-1,8,3,4,3,99])
    , TestLabel "" $ TestCase $ assertEqual "processWithICI [3,3,1108,-1,8,3,4,3,99]" [0] (processOutputWithICI [9] [3,3,1108,-1,8,3,4,3,99])
    , TestLabel "" $ TestCase $ assertEqual "processWithICI [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]" [1] (processOutputWithICI [90] [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9])
    , TestLabel "" $ TestCase $ assertEqual "processWithICI [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]" [0] (processOutputWithICI [0] [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9])
    , TestLabel "" $ TestCase $ assertEqual "processWithICI [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]" [1] (processOutputWithICI [9] [3,3,1105,-1,9,1101,0,0,12,4,12,99,1])
    , TestLabel "" $ TestCase $ assertEqual "processWithICI [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]" [0] (processOutputWithICI [0] [3,3,1105,-1,9,1101,0,0,12,4,12,99,1])
    ]


intCodeTests :: [Test]
intCodeTests = testIntCode ++ testToOperation ++ testOutputFromIntCode
