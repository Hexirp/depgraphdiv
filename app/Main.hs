module Main where
 import Prelude
 import Data.IORef

 main :: IO ()
 main = return ()

 newtype Node = Node (IORef [Node])

 newtype Graph = IORef Node
