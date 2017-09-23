-- | Provide Topological sort.
module Data.Graph.Sort where
 import Prelude
 import Data.List (unfoldr)

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

 -- | @since 0.1.0.0
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

 -- | @since 0.1.0.0
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
 -- * A list of the number of references has already sorted.
 -- * Each list of references has no duplication.
 ttsort :: Eq a => Revadlt a t -> Revadlt a t
 ttsort = map untagLength . ttsort' . map tagLength

 type Revadleti v t = Revadlet v (t, Int)

 type Revadlti v t = [Revadleti v t]
 
 type Revadltid v t = Revadlti v t -> Revadlti v t

 ttsort' :: Eq a => Revadlti a t -> Revadlti a t
 ttsort' = unfoldr go where
  go [] = Nothing
  go (x@(v :<== _) : xs) =
   Just (x, mergeRevadlti $ map (separateRevadlti v) $ splitRevadlti $ xs)

 -- | Tag the number of vertices referring to a vertex.
 tagLength :: Revadlet a t -> Revadleti a t
 tagLength (v :<== (rs, t)) = v :<== (rs, (t, length rs))

 -- | Split 'Revadlt' by the number of references.
 splitRevadlti :: Revadlti a t -> [Revadlti a t]
 splitRevadlti = unfoldr go where
  go [] = Nothing
  go (x : xs) = let (ys, zs) = sp (co x) xs in Just (x : ys, zs) where
   co (_ :<== (_, (_, n))) = n
   sp _ [] = ([], [])
   sp n (x : xs) = case n == co x of
    False -> ([], x : xs)
    True -> let (ys, zs) = sp n xs in (x : ys, zs)

 separateRevadlti
  :: Eq a => a -> Revadlti a t -> (Revadltid a t, Revadltid a t)
 separateRevadlti x = foldr go (id, id) where
  go y@(_ :<== (rs, (_, i))) (ts, fs) = delem x rs
   (\rs' -> (ts, (up y rs' i :) . fs))
   (\rs' -> ((up y rs' (i - 1) :) . ts, fs))
  up (v :<== (_, (t, _))) rs i = v :<== (rs, (t, i))

 -- | The fusion of 'delete' and 'elem' for a list without duplication.
 delem :: Eq a => a -> [a] -> ([a] -> r) -> ([a] -> r) -> r
 delem x = go where
  go [] fk _ = fk []
  go (y : ys) fk tk = case x == y of
   False -> go ys (fk . (:) y) (tk . (:) y)
   True -> tk ys

 mergeRevadlti
  :: [(Revadltid a t, Revadltid a t)] -> Revadlti a t
 mergeRevadlti = foldr go [] where
  go (ts, fs) xs = ts $ fs $ xs

 untagLength :: Revadleti a t -> Revadlet a t
 untagLength (a :<== (rs, (t, _))) = a :<== (rs, t)

