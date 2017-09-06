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
  showsPrec i (k :<= v) = showParen (prc < i) $
   showsPrec (prc + 1) k . showString " :<= " . showsPrec (prc + 1) v where
    prc = 3

 -- | 'Revadl' is a reversed adjacency list.
 type Revadl v = [Revadle v]

 -- | 'Revadlet' is a tagged 'Revadle'.
 data Revadlet v t = v :<== ([v], t)

 infix 3 :<==

 -- | /Since 0.1.0.0/
 instance (Show v, Show t) => Show (Revadlet v t) where
  showsPrec i (k :<== v) = showParen (prc < i) $
   showsPrec (prc + 1) k . showString " :<== " . showsPrec (prc + 1) v where
    prc = 3

 -- | 'Revadlt' is a tagged reversed adjacency list.
 type Revadlt v t = [Revadlet v t]

 -- | Topologically sort a graph.
 --
 -- >>> tsort [3 :<= [1,2], 2 :<= [0], 1 :<= [0], 0 :<= []]
 -- [0 :<= [],2 :<= [0],1 :<= [0],3 :<= [1,2]]
 tsort :: Eq a => Revadl a -> Revadl a
 tsort = map interpret . ttsort . normalize . map copyRef

 -- | Convert 'Revadlet' to 'Revadle'.
 interpret :: Revadlet a [a] -> Revadle a
 interpret (v :<== ([], rs)) = v :<= rs
 interpret _ = error "Found Loop"

 -- | Copy references to tags.
 copyRef :: Revadle a -> Revadlet a [a]
 copyRef (v :<= r) = (v :<== (r, r))

 -- | Topologically sort a tagged graph. It consume references and output
 -- references that were not removed because they are part of loops.
 ttsort :: Eq a => Revadlt a t -> Revadlt a t
 ttsort [] = []
 ttsort (x : xs) = let (v :<== (r, t)) = x in
  (v :<== (r, t)) : (ttsort $ normalize $ deleteRef v xs)

 -- | Sort a list of vertex in descending order of the number of vertices
 -- referenced.
 normalize :: Eq a => Revadlt a t -> Revadlt a t
 normalize = sortOn countRef

 -- | Count the number of vertices referring to a vertex.
 countRef :: Revadlet a t -> Int
 countRef (_ :<== (rs, _)) = length rs

 -- | Delete references from a vertex in a graph.
 deleteRef :: Eq a => a -> Revadlt a t -> Revadlt a t
 deleteRef x = mapRefs $ delete x

 -- | Map a function to a list of reference.
 mapRefs :: ([a] -> [a]) -> Revadlt a t -> Revadlt a t
 mapRefs = map . mapRevadlet where
  mapRevadlet f (v :<== (r, t)) = (v :<== (f r, t))
