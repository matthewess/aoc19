import Test.HUnit

import D01TyrannyOfRocketEquationTest (d01Tests)


allTests :: Test
allTests = TestList
    d01Tests
    

main :: IO Counts
main = runTestTT allTests
