import Data.List 

-- 5.1

data List a = Empty | Cons a (List a)
  deriving (Show)

toList :: [a] -> List a 
toList [] = Empty
toList (x:xs) = Cons x (toList xs)

fromList :: List a -> [a]
fromList Empty = []
fromList (Cons x xs) = x : fromList xs

-- 5.2

data Suit = Clubs | Spades | Hearts | Diamonds
    deriving (Show, Eq, Enum, Ord)

data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Enum, Ord)

data Card = Card Face Suit
    deriving (Show, Eq)

allCards :: [Card]
allCards = [ Card face suit | suit <- [Clubs .. Diamonds], face <- [Two .. Ace]]


-- 5.3

-- data Ordering = LT | EQ | GT 

cmp1 :: Card -> Card -> Ordering
cmp1 (Card f1 s1) (Card f2 s2)
  | s1 < s2   = LT
  | s1 > s2   = GT
  | otherwise = compare f1 f2

cmp2 :: Card -> Card -> Ordering
cmp2 (Card f1 s1) (Card f2 s2)
  | f1 < f2   = LT
  | f1 > f2   = GT
  | otherwise = compare s1 s2


--------------------------------------------
{-
-- Tipo interno da árvore (nota a ordem Node value left right)
data Set a = Empty
           | Node a (Set a) (Set a)
  deriving (Show, Eq)

-- empty
empty :: Set a
empty = Empty

-- insert (BST, sem rebalanceamento)
insert :: Ord a => a -> Set a -> Set a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
  | x == y    = Node y left right
  | x < y     = Node y (insert x left) right
  | otherwise = Node y left (insert x right)

-- member (pesquisa)
member :: Ord a => a -> Set a -> Bool
member _ Empty = False
member x (Node y left right)
  | x == y    = True
  | x < y     = member x left
  | otherwise = member x right

-- fromList: ordenar e construir árvore por partição (altura mínima)
fromList :: Ord a => [a] -> Set a
fromList = build . unique . sort
  where
    -- remover duplicados consecutivos (já que sort os agrupa)
    unique [] = []
    unique (z:zs) = z : unique (dropWhile (== z) zs)

    -- build a minimal-height BST from sorted (non-empty) list
    build [] = Empty
    build xs = Node x (build xsLeft) (build xsRight)
      where
        k = length xs `div` 2
        (xsLeft, x:xsRight) = splitAt k xs



-- height: altura da árvore (Empty -> 0, Node -> 1 + max ...)
height :: Set a -> Int
height Empty        = 0
height (Node _ l r) = 1 + max (height l) (height r)


--------------------------------------------

-- 5.4

size :: Set a -> Int
size Empty = 0
size (Node _ l r) = 1 + size l + size r

height :: Set a -> Int
height Empty = 0
height (Node _ l r) = 1 + max (height l) (height r)
-}
-- 5.6

type Name = Char

data Prop = Const Bool
          | Var Name
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          deriving Show

type Env = [(Name, Bool)]

eval :: Env -> Prop -> Bool
eval _ (Const b) = b
eval env (Var x) = case lookup x env of
                  Just v -> v
                  Nothing -> error ("Undefined variable")
eval env (Not p) = not (eval env p)
eval env (And p q) = eval env p && eval env q
eval env (Imply p q) = not (eval env p) || eval env q
eval env (Or p q) = eval env p || eval env q

-- lookup :: Eq a => a -> [(a,b)] -> Maybe b  // comes from the Prelude

-- 5.7

vars :: Prop -> [Name]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q

-- 5.8

booleans :: Int -> [[Bool]]
booleans 0 = [[]]
booleans n =
  [b:bs | b <- [False, True], bs <- booleans (n-1)]

-- 5.9

environments :: [Name] -> [Env]
environments ns = map (zip ns) (booleans (length ns))

-- 5.10

table :: Prop -> [(Env, Bool)]
table p =
  let ns   = names p  
      envs = environments ns        
  in [ (e, eval e p) | e <- envs ]