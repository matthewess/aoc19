{-# LANGUAGE TupleSections #-}
module D06UniversalOrbitMap where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Tuple (swap)
import Data.Maybe (fromMaybe)


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

fromEdges' :: [(Node, Node)] -> Graph
fromEdges' xs = M.fromListWith (++) . map (fmap (:[])) $ xs ++ map swap xs


main :: IO ()
main = getContents >>=
    print . totalOrbits . fromString
