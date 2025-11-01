-- 3.1

andd :: [Bool] -> Bool 
andd [] = True
andd (x:xs) 
    | x == True = andd xs
    | otherwise = False

orr :: [Bool] -> Bool
orr [] = False --se não tiver nenhum valor, suponho que seja False tudo
orr (x:xs)
    | x == True = True
    | otherwise = orr xs

concatt :: [[a]] -> [a]
concatt [] = []     -- base?
concatt [[x]] = [x]
concatt (x:xs) = x ++ concatt xs

replicatee :: Int -> a -> [a]
replicatee 0 _ = []
replicatee n x = x : replicate (n-1) x

{- Ou
replicatee :: Int -> a -> [a]
replicatee n x
  | n <= 0    = []
  | otherwise = x : replicatee (n-1) x
-}


(!!!) :: [a] -> Int -> a 
(!!!) [] _ = error "error"
(!!!) (x:xs) n = if n == 0 then x else xs !!! (n-1)

elemm :: Eq a => a -> [a] -> Bool
elemm _ [] = False
elemm i (x:xs)
    | i == x = True
    | otherwise = elemm i xs


-- 3.2

leastDiv :: Integer -> Integer
leastDiv n = leastDivAux n 2     -- 2 pq tem de ser maior que 1

leastDivAux :: Integer -> Integer -> Integer
leastDivAux n d
  | d ^2 > n = n           
  | n `mod` d == 0 = d           
  | otherwise      = leastDivAux n (d + 1)  


isPrimeFast :: Integer -> Bool
isPrimeFast n = n > 1 && leastDiv n == n


-- 3.3

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (removeAux x xs)

removeAux :: Eq a => a -> [a] -> [a]
removeAux _ [] = []
removeAux y (z:zs)
   | y == z = removeAux y zs
   | otherwise = z : removeAux y zs

-- 3.4

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse n (x:xs) = x : n : intersperse n xs


-- 3.5

insert :: Ord a => a -> [a] -> [a]
insert y [] = [y]
insert y (x:xs) 
    | y >= x = x : insert y xs
    | otherwise = y : x : xs


isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)


-- 3.6

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys


msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  =
  let (left, right) = splitAt (length xs `div` 2) xs
  in merge (msort left) (msort right)


-- 3.7

toBits :: Int -> [Int]
toBits n
  | n < 0     = error "n deve ser positivo"
  | n == 0    = [0]
  | otherwise = reverse (toBitsAux n)
  where
    toBitsAux 0 = []
    toBitsAux k = (k `mod` 2) : toBitsAux (k `div` 2)

-- 3.8

fromBits :: [Int] -> Int
fromBits [] = 0
fromBits (b:bs) = b * (2 ^ length bs) + fromBits bs

-- 3.9

divisors :: Integer -> [Integer]
divisors n = filter (\d->n `mod`d==0) [1..n]

-- 3.10

isPrimeFast :: Integer -> Bool
isPrimeFast n = n > 1 && all (\d->n`mod`d/=0) [2..floor (sqrt(fromIntegraln))]

-- 3.11

appenddd :: [a] -> [a] -> [a]
appenddd xs ys =foldr (:) ys xs 

concattt :: [[a]] -> [a]
concattt = foldr appenddd

reversee :: [a] -> [a]
reversee = foldr (\x acc -> acc ++ [x]) []

reverseee :: [a] -> [a]
reverseee = foldl (flip (:)) []

elemmm :: Eq a => a -> [a] -> Bool
elemmm a = any (==a)

-- 3.12

fromBits :: [Int] -> Int
fromBits = foldl (\acc b -> acc * 2 + b) 0

-- 3.13

group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) =
  let run  = x : takeWhile (== x) xs
      rest = dropWhile (== x) xs
  in run : group rest
