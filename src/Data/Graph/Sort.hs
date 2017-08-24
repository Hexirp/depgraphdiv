-- | Provide Topological sort for Adjacency list.
module Data.Graph.Sort where
 import Prelude
 import Data.List (sortOn, delete)

 -- | 'Pile' is composed of a vertex and vertices referring to it.
 type Pile v = (v, [v])

 -- | 'Piles' is 'Pile's.
 type Piles v = [Pile v]

 -- | 'Mountain' is composed of 'Pile' and a tag.
 type Mountain v t = (v, [v], t)

 -- | 'Mountains' is 'Mountain's.
 type Mountains v t = [Mountain v t]

 -- | 'Peak' is a vertex and a tag.
 type Peak v t = (v, t)

 -- | 'Peaks' is 'Peak's.
 type Peaks v t = [Peak v t]

 -- | Sort a graph. It's Topological sort.
 --
 -- >>> tsort [(3,[1,2]),(2,[0]),(1,[0]),(0,[])]
 -- [(0,[]),(2,[0]),(1,[0]),(3,[1,2])]
 tsort :: Eq a => Piles a -> Piles a
 tsort = tsort_main . tsort_sort . map copyRef

 -- | Copy references.
 copyRef :: Pile a -> Mountain a [a]
 copyRef (v, r) = (v, r, r)

 -- | Main part of 'tsort'.
 tsort_main :: Eq a => Mountains a t -> Peaks a t
 tsort_main [] = []
 tsort_main (x : xs) = let (v, r, t) = x in case r of
  [] -> (v, t) : (tsort_main $ tsort_sort $ tsort_delete v xs)
  _ -> error "Found Loop"

 -- | Sort a list of vertex in descending order of the number of vertices referenced.
 --
 -- >>> tsort_sort [(0,[],[]),(1,[2,3],[]),(2,[3],[]),(3,[],[])]
 -- [(0,[],[]),(3,[],[]),(2,[3],[]),(1,[2,3],[])]
 tsort_sort :: Eq a => Mountains a t -> Mountains a t
 tsort_sort = sortOn countRef

 -- | Count the number of vertices referring to a vertex.
 countRef :: Mountain a t -> Int
 countRef (_, x, _) = length x

 -- | Delete references from a vertex in a graph.
 --
 -- >>> tsort_delete 1 [(0,[],[]),(2,[1,3],[]),(3,[],[])]
 -- [(0,[],[]),(2,[3],[]),(3,[],[])]
 tsort_delete :: Eq a => a -> Mountains a t -> Mountains a t
 tsort_delete x = mapRefs (delete x)

 -- | Map a function to a list of reference.
 mapRefs :: ([a] -> [a]) -> Mountains a t -> Mountains a t
 mapRefs f = map (second f) where
  second f (x, y, z) = (x, f y, z)
