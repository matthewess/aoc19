module D07AmplificatoinCircuit where

import Control.Arrow ((&&&))
import Data.List (permutations)
import Data.List.Split (splitOn)
import System.IO.Unsafe (unsafePerformIO)

import IntCodeInterpreter
    ( IntCode(..)
    , Mode(..)
    , process
    , initIntCode
    , values
    , appendToInput
    , flushOutput
    )


-- Part I

processOutputWithICI :: [Int] -> [Int] -> [Int]
processOutputWithICI ys = outputStrip . process . initIntCode ys


f' :: [Int] -> [Int] -> [Int] -> [Int]
f' program [] input = input
f' program (x:xs) input = f' program xs output
    where output = outputStrip $ process $ initIntCode (x:input) program


calculateAmplificationForPhaseSetting :: [Int] -> [Int] -> [Int]
calculateAmplificationForPhaseSetting program phaseSetting = f' program phaseSetting [0]


allPermutations :: [[Int]]
allPermutations = permutations [0..4]


bestThrusterOutput :: [Int] -> Int
bestThrusterOutput program =
    maximum $ concatMap (calculateAmplificationForPhaseSetting program) allPermutations


-- Part II

initAmplificationArray :: [Int] -> [Int] -> [IntCode]
initAmplificationArray program phaseSetting =
    zipWith initIntCode (map (:[]) phaseSetting) (repeat program)


feedbackLoop :: [IntCode] -> [Int] -> [Int]
feedbackLoop [] xs = xs
feedbackLoop (ic:ics) lastAmpLastOutput =
    let
        ic' = process (appendToInput ic lastAmpLastOutput)
        out = outputStrip ic'
        ic'' = flushOutput ic'
    in
        case mode ic' of
            Halted -> feedbackLoop ics out
            WaitingForInput -> feedbackLoop (ics ++ [ic'']) out


calculateAmplificationForPhaseSettingWithFeedback :: [Int] -> [Int] -> [Int]
calculateAmplificationForPhaseSettingWithFeedback program phaseSetting =
    feedbackLoop (initAmplificationArray program phaseSetting) [0]

allPermutations' :: [[Int]]
allPermutations' = permutations [5..9]

bestThrusterOutputWithFeedback :: [Int] -> Int
bestThrusterOutputWithFeedback program =
    maximum $ concatMap (calculateAmplificationForPhaseSettingWithFeedback program) allPermutations'


main :: IO ()
main = getContents >>=
    print . (bestThrusterOutput &&& bestThrusterOutputWithFeedback) . map read . splitOn ","

