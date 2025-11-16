{- Worksheet 3 -}

-- 3.1

myand :: [Bool] -> Bool
myand [] = True     -- se for vazia, fica true
myand (x:xs)
    | x == False = False
    | otherwise = myand xs


myor :: [Bool] -> Bool
myor [] = False
myor (x:xs)
    | x == False = myor xs
    | otherwise = True


myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat [[x]] = [x]
myconcat (x:xs) = x ++ myconcat xs

myreplicate :: Int -> a -> [a]
myreplicate 0 _ = [] -- base case
myreplicate n x = x : myreplicate (n-1) x

myindex :: [a] -> Int -> a
myindex [] _ = error "empty list"
myindex (x:xs) n = if n == 0 then x else myindex xs (n-1)

myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem y (x:xs) 
    | y == x = True
    | otherwise = myelem y xs


-- 3.2

leastDiv :: Integer -> Integer
leastDiv n = leastDivAux n 2

leastDivAux :: Integer -> Integer -> Integer
leastDivAux n d
    | d^2 > n = n
    | n `mod` d == 0 = d
    | otherwise = leastDivAux n (d+1)


isPrimeFast :: Integer -> Bool
isPrimeFast n = n > 1 && (leastDiv n) == n


-- 3.3

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (remove x xs)
  where
    remove _ [] = []
    remove y (z:zs)
        | y == z    = remove y zs
        | otherwise = z : remove y zs


-- 3.4

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [y] = [y]
intersperse x (y:ys) = y : x : intersperse x ys


-- 3..5

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert y (x:xs)
    | y < x = y : x : xs
    | otherwise = x : insert y xs


isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)  -- lembra de como fica quando posto tudo em extenso

-- 3.6

merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) 
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
    where 
        (left, right) = splitAt (length xs `div` 2) xs


-- 3.7

toBits :: Int -> [Int]
toBits n 
    | n < 0 = error "negative"
    | n == 0 = [0]
    | otherwise = reverse (go n)
    where 
        go 0 = []
        go k = let (q,r) = quotRem k 2 in r : go q


-- 3.8 
fromBits :: [Int] -> Int
fromBits = foldl step 0
  where
    step acc b
      | b == 0 || b == 1 = acc * 2 + b
      | otherwise        = error "bits must be 0 or 1"



-- Higher Order Functions

-- 3.9

divisors :: Integer -> [Integer]
divisors n = filter (\d->n`mod`d==0) [1..n]

-- 3.10

isPrimeFastt :: Integer -> Bool
isPrimeFastt n = n > 1 && all(\d->n`mod`d/=0) [2..floor(sqrt(fromIntegral n))]



-- 3.11

appenddd :: [a] -> [a] -> [a]
appenddd xs ys = foldr (:) ys xs

concattt :: [[a]] -> [a]
concattt = foldr appenddd

reversee :: [a] -> [a]
reversee = foldr (\x acc -> acc ++ [x]) []

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