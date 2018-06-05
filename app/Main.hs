module Main where
 import Prelude
 import Data.IORef

 main :: IO ()
 main = return ()

 newtype Node = Node (IORef [Node])

 newtype Graph = IORef Node

 -- | 複数の物を入れることが出来て、重複を持たない入れ物。
 --
 -- この型はソースコードで表されない条件を持つ。
 -- 即ち、任意の型 @a : Type@ と実装 @_ : Eq a@ と値 @x : Set a@ に対して
 -- 以下の等式が常に成り立つ。
 --
 -- > overlapped x = False
 type Set a = [a]

 contain :: Eq a => a -> [a] -> Bool
 contain a = go
  where
   go []     = False
   go (x:xs) = (a == x) || go xs

 overlapped :: Eq a => [a] -> Bool
 overlapped []     = False
 overlapped (x:xs) = contain x xs || overlapped xs
