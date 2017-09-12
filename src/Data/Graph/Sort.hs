-- | Provide Topological sort.
module Data.Graph.Sort where
 import Prelude
 import Data.List (sortOn, delete, unfoldr)

 -- | 'Revadle' is a element of reversed adjacency list.
 -- It's composed of a vertex (top) and vertices referring to a top (references).
 --
 -- Show 'Revadle':
 --
 -- prop> showsPrec i (0 :<= []) s ++ s' == showsPrec i (0 :<= []) (s ++ s')
 --
 -- >>> show (0 :<= [1])
 -- "0 :<= [1]"
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
 --
 -- Show 'Revadlet':
 --
 -- prop> showsPrec i (0 :<== ([], 0)) s ++ s' == showsPrec i (0 :<== ([], 0)) (s ++ s')
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
 -- Example:
 --
 -- >>> tsort [3 :<= [], 2 :<= [3], 1 :<= [3], 0 :<= [1, 2]]
 -- [3 :<= [],2 :<= [3],1 :<= [3],0 :<= [1,2]]
 tsort :: Eq a => Revadl a -> Revadl a
 tsort = map recoverRefs . ttsort . map copyRefs

 -- | Recover references from a tag.
 recoverRefs :: Revadlet a [a] -> Revadle a
 recoverRefs (v :<== ([], rs)) = v :<= rs
 recoverRefs _ = error "Found Loop"

 -- | Copy references to a tag.
 copyRefs :: Revadle a -> Revadlet a [a]
 copyRefs (v :<= r) = v :<== (r, r)

 -- | Topologically sort a tagged graph. It consume references and output
 -- references that were not removed because they are part of loops.
 --
 -- Example:
 --
 -- >>> ttsort [0 :<== ([], "zero"), 1 :<== ([0], "one"), 2 :<== ([0, 1], "two"), 3 :<== ([0, 1, 2], "three")]
 -- [0 :<== ([],"zero"),1 :<== ([],"one"),2 :<== ([],"two"),3 :<== ([],"three")]
 --
 -- It's stable sort.
 --
 -- >>> ttsort [0 :<== ([], '0'), 3 :<== ([0], '3'), 2 :<== ([0], '2'), 1 :<== ([0], '1')]
 -- [0 :<== ([],'0'),3 :<== ([],'3'),2 :<== ([],'2'),1 :<== ([],'1')]
 --
 -- Constraint:
 --
 -- An argument are assumed to satisfy this constraints.
 --
 -- * A list of tops has no overlap.
 -- * The list does not change if 'normalize' is applied.
 -- * Each list of references has no duplication.
 ttsort :: Eq a => Revadlt a t -> Revadlt a t
 ttsort = unfoldr go where
  go [] = Nothing
  go (x@(v :<== _) : xs) = Just (x, normalize $ deleteRef v xs)
  
 -- | Tag the number of vertices referring to a vertex.
 tagLength :: Revadlet a t -> Revadlet a (t, Int)
 tagLength (v :<== (rs, t)) = v :<== (rs, (t, length rs))

 -- | Split 'Revadlt' by the number of references.
 splitRevadlt :: Revadlt a (t, Int) -> [Revadlt a (t, Int)]
 splitRevadlt = unfoldr go where
  go [] = Nothing
  go (x : xs) = let (ys, zs) = sp (co x) xs in (x : ys, zs) where
   co (_ :<== (_, (_, n))) = n
   sp _ [] = ([], [])
   sp n (x : xs) = case n == co x of
    False -> ([], x : xs)
    True -> let (ys, zs) = sp n xs in (x : ys, zs)
  
 -- | Drop references from a vertex.
 dropRef :: Eq a => a -> Revadlet a t -> (Revadlet a t, Bool)
 dropRef x (v :<== (rs, t)) = (v :<== (delete x rs, t), elem x rs)

 -- | TODO
 separateRevadlt
  :: Eq a => a -> Revadlt a (t, Int) -> (Revadlt a (t, Int), Revadlt a (t, Int))
 separateRevadlt v = go ([], []) where
  go k [] = k
  go (kf, kt) (x : xs) = let (x', d) = dropRef v x in case d of
   False -> go (kf ++ [x'], kt) xs
   True -> go (kf, kt ++ [x']) xs

 -- | TODO
 mergeRevadlt
  :: [(Revadlt a (t, Int), Revadlt a (t, Int))] -> Revadlt a (t, Int)
 mergeRevadlt [] = []
 mergeRevadlt ((xf, xt) : xs) = xf ++ xt ++ mergeRevadlt xs

 -- | TODO
 untagLength :: Revadlet a (t, Int) -> Revadlet a t
 untagLength (a :<== (rs, (t, _))) = a :<== (rs, t)

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
  mapRevadlet f (v :<== (r, t)) = v :<== (f r, t)
