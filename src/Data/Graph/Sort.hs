-- | Provide Topological sort.
module Data.Graph.Sort where
 import Prelude
 import Data.List (sortOn, delete)
 import Control.Arrow (first, second)

 -- | 'Pile' is composed of a vertex and vertices referring to it.
 type Pile v = (v, [v])

 -- | 'Mountain' is composed of 'Pile' and a tag.
 type Mountain v t = (Pile v, t)

 -- | 'Peak' is a vertex and a tag.
 type Peak v t = (v, t)

 -- | Sort a graph. It's Topological sort.
 --
 -- >>> tsort [(3,[1,2]),(2,[0]),(1,[0]),(0,[])]
 -- [(0,[]),(2,[0]),(1,[0]),(3,[1,2])]
 tsort :: Eq a => [Pile a] -> [Pile a]
 tsort = tsort_main . tsort_sort . map copyRef

 -- | Copy references to tags.
 copyRef :: Pile a -> Mountain a [a]
 copyRef (v, r) = ((v, r), r)

 -- | Main part of 'tsort'.
 tsort_main :: Eq a => [Mountain a t] -> [Peak a t]
 tsort_main [] = []
 tsort_main (x : xs) = let ((v, r), t) = x in case r of
  [] -> (v, t) : (tsort_main $ tsort_sort $ tsort_delete v xs)
  _ -> error "Found Loop"

 -- | Sort a list of vertex in descending order of the number of vertices referenced.
 --
 -- >>> tsort_sort [(0,[],[]),(1,[2,3],[]),(2,[3],[]),(3,[],[])]
 -- [(0,[],[]),(3,[],[]),(2,[3],[]),(1,[2,3],[])]
 tsort_sort :: Eq a => [Mountain a t] -> [Mountain a t]
 tsort_sort = sortOn countRef

 -- | Count the number of vertices referring to a vertex.
 countRef :: Mountain a t -> Int
 countRef = length . snd . fst

 -- | Delete references from a vertex in a graph.
 --
 -- >>> tsort_delete 1 [(0,[],[]),(2,[1,3],[]),(3,[],[])]
 -- [(0,[],[]),(2,[3],[]),(3,[],[])]
 tsort_delete :: Eq a => a -> [Mountain a t] -> [Mountain a t]
 tsort_delete x = mapRefs $ delete x

 -- | Map a function to a list of reference.
 mapRefs :: ([a] -> [a]) -> [Mountain a t] -> [Mountain a t]
 mapRefs = map . first . second
