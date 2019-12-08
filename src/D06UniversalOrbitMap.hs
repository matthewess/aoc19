{-# LANGUAGE TupleSections #-}
module D06UniversalOrbitMap where

import Control.Arrow ((&&&))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Tuple (swap)
import Data.Maybe (fromMaybe, listToMaybe)

type Node = String
type Children = [String]
type Graph = M.Map Node Children


fromEdges :: [(Node, Node)] -> Graph
fromEdges = M.fromListWith (++) . map (fmap (:[]))


fromString :: String -> Graph
fromString = fromEdges . map ((\[x, y] -> (x, y)) . splitOn ")") . lines


allOrbits :: Graph -> Node -> S.Set (Node, Node)
allOrbits g n =
    let
        children = fromMaybe [] $ g M.!? n
    in
        case children of
            [] -> S.empty
            xs ->
                let
                    allOrbitsTillNow = S.unions $ map (allOrbits g) xs
                    snds = S.map ((n,) . snd) allOrbitsTillNow
                    fsts = S.map ((n,) . fst) allOrbitsTillNow
                    childrens = S.fromList $ map (n,) children
                in
                    fsts `S.union` snds `S.union` childrens `S.union` allOrbitsTillNow


roots :: Graph -> [Node]
roots g = S.toList $ (S.fromList $ M.keys g) `S.difference` (S.fromList $ concat $ M.elems g)

totalOrbits :: Graph -> Int
totalOrbits g = S.size $ allOrbits g (head $ roots g) 


-- Part II

pathFromCenter :: Node -> Graph -> Node -> [Node]
pathFromCenter searchNode g presentNode =
    let
        children = fromMaybe [] $ g M.!? presentNode
        recPaths = map (pathFromCenter searchNode g) children
        validPaths = filter (not . null) recPaths
        maybeValidPath = listToMaybe validPaths
        pathToReturn = case maybeValidPath of
                            Nothing -> []
                            Just path -> presentNode : path
    in
        if searchNode `elem` children then [presentNode, searchNode] else pathToReturn


jumpsRequired :: Graph -> Int
jumpsRequired g =
    let
        pathToYou = pathFromCenter "YOU" g "COM"
        pathToSanta = pathFromCenter "SAN" g "COM"
        you = S.fromList pathToYou 
        san = S.fromList pathToSanta
        uniqueJumps = (san `S.union` you) `S.difference` (san `S.intersection` you)
    in
        (S.size uniqueJumps) - 2


main :: IO ()
main = getContents >>=
    print . (totalOrbits &&& jumpsRequired) . fromString
