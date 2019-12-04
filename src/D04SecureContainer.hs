module D04SecureContainer where

import Data.List (groupBy, sort)


-- Part I

adjacentDigitsAreSame :: Int -> Bool
adjacentDigitsAreSame =
    any (>=2) . map length . groupBy (==) . show


digitsIncreaseOrAreSame :: Int -> Bool
digitsIncreaseOrAreSame xs =
    show xs == (sort $ show xs)


meetsCriteria :: [Int -> Bool] -> Int -> Bool
meetsCriteria criterias x =
    and $ map ($ x) criterias


numbersInRangeMeetingCriteria :: [Int -> Bool] -> [Int] -> Int
numbersInRangeMeetingCriteria criterias =
    sum . map (fromEnum . meetsCriteria criterias)


p1 :: [Int] -> Int
p1 = numbersInRangeMeetingCriteria
    [ adjacentDigitsAreSame
    , digitsIncreaseOrAreSame
    ]


-- Part II

matchingAdjecentNumberAreNotPartOfLargerGroup :: Int -> Bool
matchingAdjecentNumberAreNotPartOfLargerGroup =
    any (==2) . map length . groupBy (==) . show


p2 :: [Int] -> Int
p2 = numbersInRangeMeetingCriteria
    [ adjacentDigitsAreSame
    , digitsIncreaseOrAreSame
    , matchingAdjecentNumberAreNotPartOfLargerGroup
    ]


main :: IO ()
main = do
    print $ p1 [206938..679128]
    print $ p2 [206938..679128]
