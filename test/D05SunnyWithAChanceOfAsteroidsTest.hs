module D05SunnyWithAChanceOfAsteroidsTest where

import Test.HUnit
import D05SunnyWithAChanceOfAsteroids (processOutputWithICI)


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


d05Tests :: [Test]
d05Tests = testOutputFromIntCode
