-- | Provide Graph by simple interaction. It's like <http://manual.cytoscape.org/en/stable/Supported_Network_File_Formats.html#sif-format SIF format>.
module Data.Graph.Simple where
 import Prelude

 -- | 'Edge' is composed of a start vertex and an end vertex: @(start, end)@.
 type Edge a = (a, a)

 -- | 'Graph' is composed of 'Edge's.
 newtype Graph a = Graph [Edge a]

 -- | 'Adjacency' is composed of a vertex and a vertices referenced by it.
 type Adjacency a = (a, [a])

 -- | 'Adjagraph' is composed of a list of 'Adjacency'.
 newtype Adjagraph a = Adjagraph [Adjacency a]

 tsort :: Ord a => Graph a -> Adjagraph a
 tsort = undefined
