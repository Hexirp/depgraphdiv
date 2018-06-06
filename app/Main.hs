{-# LANGUAGE LambdaCase #-}

module Main where
 import Prelude
 import Data.IORef

 main :: IO ()
 main = return ()

 -- | 複数の物を参照する物。
 newtype Node = Node { unNode :: IORef (Set Node)}

 references :: Node -> IO (Set Node)
 references = readIORef (unNode a)

 landscape :: Node -> IO (Set Node)
 landscape a = landscapeCum a []

 landscapeCum :: Node -> Set Node -> IO (Set Node)
 landscapeCum a cum = do
  refs <- references a
  case refs of
   []     ->
    return cum
   (x:xs) -> do
    cums <- landscapesCum xs cum
    case contain x cums of
     False -> landscapeCum x (x : cums)
     True  -> return cums
 
 landscapesCum :: Set Node -> Set Node -> IO (Set Node)
 landscapesCum []     cum = return cum
 landscapesCum (x:xs) cum = landscapeCum x (landscapesCum xs cum)

 -- | 浅い同値。
 instance Eq Node where
  a == b = unNode a == unNode a

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
