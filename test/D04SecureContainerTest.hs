module D04SecureContainerTest where

import D04SecureContainer
    ( meetsCriteria
    , adjacentDigitsAreSame
    , digitsIncreaseOrAreSame
    , matchingAdjecentNumberAreNotPartOfLargerGroup
    )

import Test.HUnit


meetsCriteria1 = meetsCriteria [adjacentDigitsAreSame, digitsIncreaseOrAreSame]
meetsCriteria2 = meetsCriteria [adjacentDigitsAreSame, digitsIncreaseOrAreSame, matchingAdjecentNumberAreNotPartOfLargerGroup]


testMeetsCriteria =
    [ TestLabel "" $ TestCase $ assertEqual "" True (meetsCriteria1 111111)
    , TestLabel "" $ TestCase $ assertEqual "" False (meetsCriteria1 223450)
    , TestLabel "" $ TestCase $ assertEqual "" False (meetsCriteria1 123789)
    ]


testMeetsCriteria' =
    [ TestLabel "" $ TestCase $ assertEqual "" True (meetsCriteria2 112233)
    , TestLabel "" $ TestCase $ assertEqual "" False (meetsCriteria2 123444)
    , TestLabel "" $ TestCase $ assertEqual "" True (meetsCriteria2 111122)
    ]


d04Tests :: [Test]
d04Tests = testMeetsCriteria ++ testMeetsCriteria'
