module IntCodeInterpreterTest where

import IntCodeInterpreter
    ( Operation(..)
    , ParameterMode(..)
    , toOperation
    )

import Test.HUnit


testToOperation =
    [ TestLabel "" $ TestCase $ assertEqual "toInstruction 1002" (Mult Position Position) (toOperation 2)
    ]

intCodeTests :: [Test]
intCodeTests = testToOperation
