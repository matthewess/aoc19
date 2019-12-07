module IntCodeInterpreterTest where

import IntCodeInterpreter
    ( processWithICI
    , Operation(..)
    , ParameterMode(..)
    , toOperation
    )

import Test.HUnit


testIntCode =
    [ TestLabel "" $ TestCase $ assertEqual "processWithICI [1,0,0,0,99]" [2,0,0,0,99] (processWithICI [1,0,0,0,99])
    , TestLabel "" $ TestCase $ assertEqual "processWithICI [2,3,0,3,99]" [2,3,0,6,99] (processWithICI [2,3,0,3,99])
    , TestLabel "" $ TestCase $ assertEqual "processWithICI [2,4,4,5,99,0]" [2,4,4,5,99,9801] (processWithICI [2,4,4,5,99,0])
    , TestLabel "" $ TestCase $ assertEqual "processWithICI [1,1,1,4,99,5,6,0,99]" [30,1,1,4,2,5,6,0,99] (processWithICI [1,1,1,4,99,5,6,0,99])
    , TestLabel "" $ TestCase $ assertEqual "processWithICI [3,0,4,0,99]" [1,0,4,0,99] (processWithICI [3,0,4,0,99])
    ]


testToOperation =
    [ TestLabel "" $ TestCase $ assertEqual "toInstruction 1002" (Mult Position Position) (toOperation 2)
    ]

intCodeTests :: [Test]
intCodeTests = testIntCode ++ testToOperation
