module D01TyrannyOfRocketEquation where

import Control.Arrow ((&&&))


-- Part I

fuelRequirementForOneShip :: Integer -> Integer
fuelRequirementForOneShip mass =
    mass `div` 3 - 2

totalFuelRequirement :: [Integer] -> Integer
totalFuelRequirement =
    sum . map fuelRequirementForOneShip


-- Part II

recursiveFuelRequirementForOneShip :: Integer -> Integer
recursiveFuelRequirementForOneShip =
    sum . tail . takeWhile (> 0) . iterate fuelRequirementForOneShip


totalFuelRequirement' :: [Integer] -> Integer
totalFuelRequirement' =
    sum . map recursiveFuelRequirementForOneShip


main :: IO ()
main = getContents >>=
    putStr . show . (totalFuelRequirement &&& totalFuelRequirement') . map read . lines
