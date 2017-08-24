-- | Provide Topological sort for Adjacency list.
module Data.Graph.Sort where
 import Prelude
 import Data.List (sortOn, delete)

 -- | Sort a graph. It's Topological sort.
 --
 -- >>> tsort [(3,[1,2]),(2,[0]),(1,[0]),(0,[])]
 -- [(0,[]),(2,[0]),(1,[0]),(3,[1,2])]
 tsort :: Eq a => [(a, [a])] -> [(a, [a])]
 tsort = tsort_main . tsort_sort . map copyRef

 -- | Copy references.
 copyRef :: (a, [a]) -> (a, [a], [a])
 copyRef (v, r) = (v, r, r)

 -- | Main part of 'tsort'.
 tsort_main :: Eq a => [(a, [a], [a])] -> [(a, [a])]
 tsort_main [] = []
 tsort_main (x : xs) = let (v, r, r') = x in case r of
  [] -> (v, r') : (tsort_main $ tsort_sort $ tsort_delete v xs)
  _ -> error "Found Loop"

 -- | Sort a list of vertex in descending order of the number of vertices referenced.
 --
 -- >>> tsort_sort [(0,[].[]),(1,[2,3],[]),(2,[3],[]),(3,[],[])]
 -- [(0,[],[]),(3,[],[]),(2,[3],[]),(1,[2,3],[])]
 tsort_sort :: Eq a => [(a, [a], [a])] -> [(a, [a], [a])]
 tsort_sort = sortOn countRef

 -- | Count the number of vertices referring to a vertex.
 countRef :: (a, [a], [a]) -> Int
 countRef (_, x, _) = length x

 -- | Delete references from a vertex in a graph.
 --
 -- >>> tsort_delete 1 [(0,[],[]),(2,[1,3],[]),(3,[],[])]
 -- [(0,[],[]),(2,[3],[]),(3,[],[])]
 tsort_delete :: Eq a => a -> [(a, [a],[a])] -> [(a, [a],[a])]
 tsort_delete x = mapRefs (delete x)

 -- | Map a function to a list of reference.
 mapRefs :: ([a] -> [a]) -> [(a, [a], [a])] -> [(a, [a], [a])]
 mapRefs f = map (second f) where
  second f (x, y, z) = (x, f y, z)
