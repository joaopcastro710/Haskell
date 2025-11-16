{- Worksheet2 -}

-- 2.1

-- conditional expressions

classify :: Int -> String
classify n = if n <= 9 then "failed"
            else if n >=10 && n <= 12 then "passed"
            else if n >=13 && n <= 15 then "good"
            else if n >=16 && n <= 18 then "very good"
            else if n >=19 && n <= 20 then "excellent"
            else "invalid"

-- guards

classifyy :: Int -> String
classifyy n 
        | n <= 9 = "failed"
        | n <= 12 = "passed"
        | n <= 15 = "good"
        | n <= 18 = "very good"
        | n <= 20 = "excellent"
        | otherwise = "invalid"

-- 2.2

classifyBMI :: Float -> Float -> String
classifyBMI w h 
        | bmi < 18.5 = "underweight"
        | bmi <= 25 = "normal weight"
        | bmi <= 30 = "overweight"
        | otherwise = "obese"
        where bmi = w/(h^2)

-- 2.3

max3 :: Ord a => a -> a -> a -> a
max3 a b c 
    | a > b && a > c = a
    | b > c = b
    | otherwise = c

min3 :: Ord a => a -> a -> a -> a
min3 a b c 
    | a < b && a < c = a
    | b < c = b
    | otherwise = c

-- 2.4

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor True True = False
xor False False = False 


-- 2.5 

-- podia ter usado a 'null' do prelude, mas usei a minha implementação

nulll :: [a] -> Bool
nulll xs = case xs of
            [] -> True
            (_:_) -> False

--conditional
safetail :: [a] -> [a]
safetail xs = if nulll xs then [] else tail xs

--guards
safetaill :: [a] -> [a]
safetaill xs 
    | nulll xs = []
    | otherwise = tail xs 

--patterns
safetailll :: [a] -> [a]
safetailll [] = []
safetailll (_:xs) = xs


-- 2.6
--using length
short :: [a] -> Bool
short x = if len < 3 then True
        else False
        where len = length x

--using multiple equations

shortt :: [a] -> Bool
shortt [] = True
shortt (_:[]) = True
shortt (_:_:[]) = True 
shortt (_:_:xs) = False


-- 2.7
median1 :: Ord a => a -> a -> a -> a
median1 x y z 
    | x <= y && y <= z = y
    | z <= y && y <= x = y
    | y <= x && x <= z = x
    | z <= x && x <= y = x
    | x <= z && z <= y = z
    | otherwise = z


median2 :: (Ord a, Num a) => a -> a -> a -> a
median2 x y z = s - m - mi
  where
    s  = x + y + z
    m  = max3 x y z
    mi = min3 x y z

-- List Comprehensions

-- 2.8

propDivs :: Integer -> [Integer] 
propDivs n = [d | d <- [1..n-1], n`mod`d == 0]

-- 2.9

perfects :: Integer -> [Integer]
perfects limit = [d | d <- [1..limit], sum(propDivs d) == d]

-- 2.10

pyths :: Integer -> [(Integer, Integer, Integer)]
pyths limit = [(x,y,z) | x <- [1..limit], y <- [1..limit], z <- [1..limit], x^2+y^2==z^2]

-- 2.11

isPrime :: Integer -> Bool
isPrime n = length ([d | d <- [1..n], n`mod`d==0]) == 2
   -- where x = [d | d <- [1..n], n`mod`d==0]

-- 2.12

myConcat :: [[a]] -> [a]
myConcat xss = [ x | xs <- xss, x <- xs ]

myReplicate :: Int -> a -> [a]
myReplicate n x = [ x | _ <- [1 .. n] ]

myIndex :: [a] -> Int -> a
myIndex xs n = head [ x | (i, x) <- zip [0..] xs, i == n ]
