module D021202ProgramAlarmTest where

import D021202ProgramAlarm (intCode)

import Test.HUnit


testIntCode =
    [ TestLabel "" $ TestCase $ assertEqual "intCode [1,0,0,0,99]" [2,0,0,0,99] (intCode [1,0,0,0,99])
    , TestLabel "" $ TestCase $ assertEqual "intCode [2,3,0,3,99]" [2,3,0,6,99] (intCode [2,3,0,3,99])
    , TestLabel "" $ TestCase $ assertEqual "intCode [2,4,4,5,99,0]" [2,4,4,5,99,9801] (intCode [2,4,4,5,99,0])
    , TestLabel "" $ TestCase $ assertEqual "intCode [1,1,1,4,99,5,6,0,99]" [30,1,1,4,2,5,6,0,99] (intCode [1,1,1,4,99,5,6,0,99])
    ]


d02Tests :: [Test]
d02Tests = testIntCode
