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
 type Revadlet v t = (v, ([v], t))

 -- | 'Revadlt' is a tagged reversed adjacency list.
 type Revadlt v t = [Revadlet v t]

 -- | Topologically sort a graph.
 --
 -- >>> tsort [3 :<= [1,2], 2 :<= [0], 1 :<= [0], 0 :<= []]
 -- [0 :<= [],2 :<= [0],1 :<= [0],3 :<= [1,2]]
 tsort :: Eq a => Revadl a -> Revadl a
 tsort = map fromRevadlet . ttsort . normalize . map copyRef

 -- | Convert 'Revadlet' to 'Revadle'.
 fromRevadlet :: Revadlet a [a] -> Revadle a
 fromRevadlet (v, ([], rs)) = v :<= rs
 fromRevadlet _ = error "Found Loop"

 -- | Copy references to tags.
 copyRef :: Revadle a -> Revadlet a [a]
 copyRef (v :<= r) = (v, (r, r))

 -- | Topologically sort a tagged graph. It consume references and output
 -- references that were not removed because they are part of loops.
 ttsort :: Eq a => Revadlt a t -> Revadlt a t
 ttsort [] = []
 ttsort (x : xs) = let (v, (r, t)) = x in
  (v, (r, t)) : (ttsort $ normalize $ deleteRef v xs)

 -- | Sort a list of vertex in descending order of the number of vertices
 -- referenced.
 normalize :: Eq a => Revadlt a t -> Revadlt a t
 normalize = sortOn countRef

 -- | Count the number of vertices referring to a vertex.
 countRef :: Revadlet a t -> Int
 countRef (_, (rs, _)) = length rs

 -- | Delete references from a vertex in a graph.
 deleteRef :: Eq a => a -> Revadlt a t -> Revadlt a t
 deleteRef x = mapRefs $ delete x

 -- | Map a function to a list of reference.
 mapRefs :: ([a] -> [a]) -> Revadlt a t -> Revadlt a t
 mapRefs = map . mapRevadlet where
  mapRevadlet f (v, (r, t)) = (v, (f r, t))
