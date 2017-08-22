-- | Provide Topological sort for Adjacency list.
module Data.Graph.Sort where
 import Prelude

 -- | Sort a graph. It's Topological sort.
 tsort :: Ord a => [(a, [a])] -> [(a, [a])]
 tsort = undefined
