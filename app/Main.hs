{-# LANGUAGE LambdaCase #-}

module Main where
 import Prelude
 import Control.Monad
 import Data.IORef

 main :: IO ()
 main = return ()

 -- | 複数の物を参照する物。
 newtype Node = Node { unNode :: IORef (Set Node) }

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
    landscapesCum refs (a : cum) -- @a@ は @cum@ に含まれない
   True -> return cum
 
 -- | ある 'Node' 群から参照を辿って行って到達できる 'Node' の集合を
 -- 与えられた集合に積み重ねた集合。
 landscapesCum :: Set Node -> Set Node -> IO (Set Node)
 landscapesCum []     cum = return cum
 landscapesCum (x:xs) cum = join $ landscapeCum x <$> landscapesCum xs cum

 -- | 浅い同値。
 instance Eq Node where
  a == b = unNode a == unNode a

 -- | ある一塊になった参照しあう複数の物。
 --
 -- 一塊になっているということは、参照を辿っていって到達できるすべての物が
 -- 元々の塊に含まれていることである。
 --
 -- 即ち、任意の値 @x : Graph@ に対して以下が常に成り立つ。
 --
 -- > closed x = return True
 newtype Graph = Graph { unGraph :: Set Node }

 closed :: Graph -> IO Bool
 closed = f . unGraph
  where
   f a = eq a <$> landscapes a

 hasLoop :: Graph -> IO Bool
 hasLoop x = (\(a, _) -> a) <$> f (unGraph x) []
  where
   f :: Set Node -> Set Node -> IO (Bool, Set Node)
   f []     cum = return (False, cum)
   f (x:xs) cum = join $ (\(xp, cum') -> (\(xpp, cum'') -> (xpp || xp, cum'')) <$> g x cum') <$> f xs cum

   g :: Node -> Set Node -> IO (Bool, Set Node)
   g a cum =
    case contain a cum of
     False -> do
      refs <- references a
      f refs (a : cum)
     True -> return (True, cum)

 -- | 複数の物を入れることが出来て、順序と重複を持たない入れ物。
 --
 -- 任意の型 @a : Type@ と実装 @_ : Eq a@ と値 @x : Set a@ に対して
 -- 以下が常に成り立つ。
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

 add :: Eq a => a -> Set a -> Set a
 add a = go
  where
   go []     = a : []
   go (x:xs) = case a == x of
    False -> x : go xs
    True  -> x : xs

 remove :: Eq a => a -> Set a -> Set a
 remove a = go
  where
   go []     = []
   go (x:xs) = case a == x of
    False -> x : go xs
    True  -> xs

 removes :: Eq a => Set a -> Set a -> Set a
 removes []     y = y
 removes (x:xs) y = remove x (removes xs y)

 sub :: Eq a => Set a -> Set a -> Bool
 sub x y = removes x y == []

 eq :: Eq a => Set a -> Set a -> Bool
 eq x y = sub x y && sub y x
