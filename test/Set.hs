module Set (Set(..), empty, fromList, member, insert) where


data Set a
  = Empty
  | Node a (Set a) (Set a)
  deriving (Show)

-- empty set
empty :: Set a
empty = Empty

-- check membership
member :: Ord a => a -> Set a -> Bool
member _ Empty = False
member x (Node y left right)
  | x < y     = member x left
  | x > y     = member x right
  | otherwise = True

-- insert element
insert :: Ord a => a -> Set a -> Set a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
  | x < y     = Node y (insert x left) right
  | x > y     = Node y left (insert x right)
  | otherwise = Node y left right

-- build a set from a list
fromList :: Ord a => [a] -> Set a
fromList = foldr insert empty
