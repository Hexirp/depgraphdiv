-- | Provide Topological sort.
module Data.Graph.Sort where
 import Prelude
 import Data.List (sortOn, delete)
 import Control.Arrow (first, second)

 -- | 'Revadle' is a element of reversed adjacency list.
 type Revadle v = (v, [v])

 -- | Make 'Revadle'.
 (<+) :: v -> [v] -> Revadle v
 v <+ rs = (v, rs)

 infix 3 <+

 -- | Convert 'Revadle' to 'ShowS' with a precedence.
 --
 -- prop> showsPrecRevadle i (0 <+ []) s ++ s' == showsPrec i (0 <+ []) (s ++ s')
 showsPrecRevadle :: Show v => Int -> Revadle v -> String -> String
 showsPrecRevadle i (v, rs) = showParen (i > prec) $
  showsPrec (prec + 1) v . showString " <+ " . showsPrec (prec + 1) rs where
   prec = 3

 -- | 'Revadl' is a reversed adjacency list.
 type Revadl v = [Revadle v]

 -- | 'Revadlet' is composed of 'Revadle' and a tag.
 type Revadlet v t = (Revadle v, t)

 -- | 'Revadlt' is a tagged reversed adjacency list.
 type Revadlt v t = [Revadlet v t]

 -- | 'Vertext' is a vertex and a tag.
 type Vertext v t = (v, t)

 -- | 'Vertextl' is a list of 'Vertext'.
 type Vertextl v t = [Vertext v t]

 -- | Sort a graph. It's Topological sort.
 --
 -- >>> tsort [3 <+ [1,2], 2 <+ [0], 1 <+ [0], 0 <+ []]
 -- [(0,[]),(2,[0]),(1,[0]),(3,[1,2])]
 tsort :: Eq a => Revadl a -> Revadl a
 tsort = tsort_main . tsort_sort . map copyRef

 -- | Copy references to tags.
 copyRef :: Revadle a -> Revadlet a [a]
 copyRef (v, r) = ((v, r), r)

 -- | Main part of 'tsort'.
 tsort_main :: Eq a => Revadlt a t -> Vertextl a t
 tsort_main [] = []
 tsort_main (x : xs) = let ((v, r), t) = x in case r of
  [] -> (v, t) : (tsort_main $ tsort_sort $ tsort_delete v xs)
  _ -> error "Found Loop"

 -- | Sort a list of vertex in descending order of the number of vertices referenced.
 --
 -- >>> tsort_sort [((0,[]),[]),((1,[2,3]),[]),((2,[3]),[]),((3,[]),[])]
 -- [((0,[]),[]),((3,[]),[]),((2,[3]),[]),((1,[2,3]),[])]
 tsort_sort :: Eq a => Revadlt a t -> Revadlt a t
 tsort_sort = sortOn countRef

 -- | Count the number of vertices referring to a vertex.
 countRef :: Revadlet a t -> Int
 countRef = length . snd . fst

 -- | Delete references from a vertex in a graph.
 --
 -- >>> tsort_delete 1 [((0,[]),[]),((2,[1,3]),[]),((3,[]),[])]
 -- [((0,[]),[]),((2,[3]),[]),((3,[]),[])]
 tsort_delete :: Eq a => a -> Revadlt a t -> Revadlt a t
 tsort_delete x = mapRefs $ delete x

 -- | Map a function to a list of reference.
 mapRefs :: ([a] -> [a]) -> Revadlt a t -> Revadlt a t
 mapRefs = map . first . second
