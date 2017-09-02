-- | Provide Topological sort.
module Data.Graph.Sort where
 import Prelude
 import Data.List (sortOn, delete)

 -- | 'Revadle' is a element of reversed adjacency list.
 --
 -- Show 'Revadle':
 -- prop> showsPrec i (0 :<= []) s ++ s' == showsPrec i (0 :<= []) (s ++ s')
 data Revadle v = v :<= [v]

 infix 3 :<=

 -- | /Since 0.1.0.0/
 instance Show v => Show (Revadle v) where
  showsPrec i (v :<= rs) = showParen (i > prec) $
   showsPrec (prec + 1) v . showString " :<= " . showsPrec (prec + 1) rs where
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

 -- | Topologically sort a graph.
 --
 -- >>> tsort [3 :<= [1,2], 2 :<= [0], 1 :<= [0], 0 :<= []]
 -- [0 :<= [],2 :<= [0],1 :<= [0],3 :<= [1,2]]
 tsort :: Eq a => Revadl a -> Revadl a
 tsort = map fromVertext . gsort . tsort_sort . map copyRef

 -- | Convert 'Vertext' to 'Revadle'.
 fromVertext :: Vertext a [a] -> Revadle a
 fromVertext (v, rs) = v :<= rs

 -- | Copy references to tags.
 copyRef :: Revadle a -> Revadlet a [a]
 copyRef (v :<= r) = ((v :<= r), r)

 -- | Sort a graph by consuming references.
 gsort :: Eq a => Revadlt a t -> Vertextl a t
 gsort [] = []
 gsort (x : xs) = let ((v :<= r), t) = x in case r of
  [] -> (v, t) : (gsort $ tsort_sort $ tsort_delete v xs)
  _ -> error "Found Loop"

 -- | Sort a list of vertex in descending order of the number of vertices referenced.
 --
 -- >>> tsort_sort [(0 :<= [], []), (1 :<= [2,3], []), (2 :<= [3], []), (3 :<= [], [])]
 -- [(0 :<= [],[]),(3 :<= [],[]),(2 :<= [3],[]),(1 :<= [2,3],[])]
 tsort_sort :: Eq a => Revadlt a t -> Revadlt a t
 tsort_sort = sortOn countRef

 -- | Count the number of vertices referring to a vertex.
 countRef :: Revadlet a t -> Int
 countRef ((_ :<= rs), _) = length rs

 -- | Delete references from a vertex in a graph.
 --
 -- >>> tsort_delete 1 [(0 :<= [], []), (2 :<= [1,3], []), (3 :<= [], [])]
 -- [(0 :<= [],[]),(2 :<= [3],[]),(3 :<= [],[])]
 tsort_delete :: Eq a => a -> Revadlt a t -> Revadlt a t
 tsort_delete x = mapRefs $ delete x

 -- | Map a function to a list of reference.
 mapRefs :: ([a] -> [a]) -> Revadlt a t -> Revadlt a t
 mapRefs = map . mapRev where
  mapRev f ((v :<= r), t) = ((v :<= f r), t)
