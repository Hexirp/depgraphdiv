{-# LANGUAGE LambdaCase #-}

module Main where
 import Prelude
 import Data.IORef

 main :: IO ()
 main = return ()

 -- | 複数の物を参照する物。
 newtype Node = Node { unNode :: IORef (Set Node)}

 -- | ある 'Node' が参照する 'Node' の集合。
 references :: Node -> IO (Set Node)
 references a = readIORef (unNode a)

 -- 'landscape' と 'landscapes' 、 'landscapeCum' と 'landscapesCum' は
 -- 一対である。
 --
 -- それは 'Node' が 'Set' と 'Node' との二重帰納型であるためだ。
 -- 'landscapeCum' と 'landscapesCum' はどちらも第二引数が状態でありる。
 -- それぞれ 'Node' と 'Set' に対応し、状態を変化させながら再帰しあう。
 --
 -- その状態は集合であり参照を辿り終えた 'Node' を表す。
 --
 -- 'landscapeCum' の実装は注意を要する。
 -- @landscapeCum a cum@ の計算を考えてみよう。
 -- まず @cum@ が @a@ を含むかどうか考える。
 -- 含むのならば、 @a@ に到達しているということであり、
 -- 即ち、 @a@ からの参照はすべて辿り終えている。
 -- そのため、そのまま何もせず @cum@ を返す。
 -- 含まないのならば @cum@ に @a@ を辿り終えたと記録した後、
 -- @a@ が参照する全ての 'Node' を辿っていく。

 -- | ある 'Node' から参照を辿って行って到達できる 'Node' の集合。
 landscape :: Node -> IO (Set Node)
 landscape a = landscapeCum a []

 -- | ある 'Node' 群から参照を辿って行って到達できる 'Node' の集合。
 landscapes :: Set Node -> IO (Set Node)
 landscapes x = landscapesCum x []

 -- | ある 'Node' から参照を辿って行って到達できる 'Node' の集合を
 -- 与えられた集合に積み重ねた集合。
 landscapeCum :: Node -> Set Node -> IO (Set Node)
 landscapeCum a cum =
  case contain a cum of
   False -> do
    refs <- references a
    landscapesCum refs (a : cum)
   True -> return cum
 
 -- | ある 'Node' 群から参照を辿って行って到達できる 'Node' の集合を
 -- 与えられた集合に積み重ねた集合。
 landscapesCum :: Set Node -> Set Node -> IO (Set Node)
 landscapesCum []     cum = return cum
 landscapesCum (x:xs) cum = landscapeCum x =<< landscapesCum xs cum

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
