import Set
import Data.List

-- Algebric data types
{-
-- 5.1

data List a = Empty | Cons a (List a)
    deriving (Show)

{-
Empty Corresponde a []
Cons x xs corresponde a x : xs      [1,2,3] = Cons 1 (Cons 2 (Cons 3 Empty))
-}

toList :: [a] -> List a
toList [] = Empty
toList (x:xs) = Cons x (toList xs)

fromListt :: List a -> [a]
fromListt Empty = []
fromListt (Cons x xs)  = x : fromListt xs


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


-}
-- Search Trees and Syntax Trees

-- 5.4 

type Name = Char
type Env  = [(Name, Bool)]

size :: Set a -> Int
size Empty = 0
size (Node _ l r ) = 1 + size l + size r

height :: Set a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)


{-
set1 = foldr insert empty [1..1000]
set2 = fromList [1..1000]

No 1, com a lista está ordenada crescentemente, e insert põe tudo sempre á direita, a árvore fica quase uma lista 
guida. Complexidade O(n).
No 2, fromList faz um sort e contrói uma árvore balanceada pela mediana. O (log n). Mais rápida.
-}


-- 5.6


data Prop = Const Bool
            | Var Name
            | Not Prop
            | And Prop Prop
            | Imply Prop Prop
            | Or Prop Prop
            deriving Show

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
booleans n = [b:bs | b <- [False, True], bs <- booleans (n-1)]

-- 5.9

environments :: [Name] -> [Env]
environments ns = map (zip ns) (booleans (length ns))

-- 5.10

table :: Prop -> [(Env, Bool)]
table p =
  let ns   = names p  
      envs = environments ns        
  in [ (e, eval e p) | e <- envs ]