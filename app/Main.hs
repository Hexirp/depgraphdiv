module Main where
 import Prelude
 import Data.IORef

 main :: IO ()
 main = return ()

 newtype Node = Node (IORef (Set Node))

 -- | ある一塊になった参照しあう複数の物。
 --
 -- 一塊になっているということは、参照を辿っていって到達できるすべての物が
 -- 元々の塊に含まれていることである。
 newtype Graph = Graph (Set Node)

 -- | 複数の物を入れることが出来て、順序と重複を持たない入れ物。
 --
 -- 任意の型 @a : Type@ と実装 @_ : Eq a@ と値 @x : Set a@ に対して
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
