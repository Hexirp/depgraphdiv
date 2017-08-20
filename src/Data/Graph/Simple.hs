-- | Provide Graph by simple interaction. It's like <http://manual.cytoscape.org/en/stable/Supported_Network_File_Formats.html#sif-format SIF format>.
module Data.Graph.Simple where
 import Prelude

 -- | 'Edge' is composed of a start vertex and an end vertex: @(start, end)@.
 type Edge a = (a, a)

 -- | Make 'Edge'.
 (=:>) :: a -> a -> Edge a
 x =:> y = (x, y)

 infix 1 =:>

 -- | Show 'Edge' according to order of operations. It's 'showsPrec' for 'Edge'.
 --
 -- >>> showsPrecEdge 0 (3, 4) []
 -- "3 =:> 4"
 --
 -- >>> showsPrecEdge 1 (3, 4) []
 -- "3 =:> 4"
 --
 -- >>> showsPrecEdge 2 (3, 4) []
 -- "(3 =:> 4)"
 showsPrecEdge :: Show a => Int -> Edge a -> ShowS
 showsPrecEdge i (x, y) = showParen (i > infix_edge) $
  id
   . showsPrec (infix_edge + 1) x
   . showString " =:> "
   . showsPrec (infix_edge + 1) y
    where infix_edge = 1

 -- | 'Graph' is composed of 'Edge's.
 newtype Graph a = Graph [Edge a]

 instance Show a => Show (Graph a) where
  showsPrec _ (Graph []) = showString "{}"
  showsPrec _ (Graph (x : xs)) = showChar '{' . go x xs where
   go x [] = showsPrecEdge 0 x . showChar '}'
   go x (y : ys) = showsPrecEdge 0 x . showString ", " . go y ys

 -- | 'Adjacency' is composed of a vertex and a vertices referenced by it.
 type Adjacency a = (a, [a])

 -- | 'Adjagraph' is composed of a list of 'Adjacency'.
 newtype Adjagraph a = Adjagraph [Adjacency a]

 tsort :: Ord a => Graph a -> Adjagraph a
 tsort = undefined
