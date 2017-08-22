-- | Provide Topological sort for Adjacency list.
module Data.Graph.Sort where
 import Prelude

 -- | Sort a graph. It's Topological sort.
 tsort :: Eq a => [(a, [a])] -> [(a, [a])]
 tsort [] = []
 tsort (x : xs) = let (v, vs) = x in case vs of
  [] -> (v, []) : tsort (tsort_sort (tsort_delete v xs))
  _ -> error "Found Loop"

 -- | Sort a list of vertex in descending order of the number of vertices referenced.
 --
 -- >>> tsort_sort [(0, []), (1, [2, 3]), (2, [3]), (3, [])]
 -- [(0, []), (3, []), (2, [3]), (1, [2, 3])]
 tsort_sort :: Eq a => [(a, [a])] -> [(a, [a])]
 tsort_sort = undefined

 -- | Delete references from a vertex in a graph.
 --
 -- >>> tsort_delete 1 [(0, []), (2, [1, 3]), (3, [])]
 -- [(0, []), (2, [3]), (3, [])]
 tsort_delete :: Eq a => a -> [(a, [a])] -> [(a, [a])]
 tsort_delete = undefined
