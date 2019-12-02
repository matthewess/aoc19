module D01TyrannyOfRocketEquationTest where

import Test.HUnit
import D01TyrannyOfRocketEquation (fuelRequirementForOneShip, recursiveFuelRequirementForOneShip)


testFuelRequirementForOneShip =
    [ TestLabel "" $ TestCase $ assertEqual "fuelRequirementForOneShip 12" 2 (fuelRequirementForOneShip 12)
    , TestLabel "" $ TestCase $ assertEqual "fuelRequirementForOneShip 14" 2 (fuelRequirementForOneShip 14)
    , TestLabel "" $ TestCase $ assertEqual "fuelRequirementForOneShip 1969" 654 (fuelRequirementForOneShip 1969)
    , TestLabel "" $ TestCase $ assertEqual "fuelRequirementForOneShip 100756" 33583 (fuelRequirementForOneShip 100756)
    ]


testRecursiveFuelRequirementForOneShip =
    [ TestLabel "" $ TestCase $ assertEqual "recursiveFuelRequirementForOneShip 12" 2 (recursiveFuelRequirementForOneShip 12)
    , TestLabel "" $ TestCase $ assertEqual "recursiveFuelRequirementForOneShip 14" 2 (recursiveFuelRequirementForOneShip 14)
    , TestLabel "" $ TestCase $ assertEqual "recursiveFuelRequirementForOneShip 1969" 966 (recursiveFuelRequirementForOneShip 1969)
    , TestLabel "" $ TestCase $ assertEqual "recursiveFuelRequirementForOneShip 100756" 50346 (recursiveFuelRequirementForOneShip 100756)
    ]

 

d01Tests :: [Test]
d01Tests = testFuelRequirementForOneShip ++ testRecursiveFuelRequirementForOneShip
    
