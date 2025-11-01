import Test.QuickCheck

-- 2.1

classify :: Int -> String
classify grade = if grade <= 9 then "failed"
                else if grade >= 10 && grade <= 12 then "passed"
                else if grade >= 13 && grade <= 15 then "good"
                else if grade >= 16 && grade <= 18 then "very good"
                else if grade >= 19 && grade <= 20 then "excellent"
                else "invalid"


classifyy :: Int -> String
classifyy grade
    | grade <= 9 = "failed"
    | {-grade >= 10 &&-} grade <= 12 = "passed"
    | {-grade >= 13 &&-} grade <= 15 = "good"
    | {-grade >= 16 &&-} grade <= 18 = "very good"
    | {-grade >= 19 &&-} grade <= 20 = "excellent"
    | otherwise = "Invalid"


-- 2.2

classifyBMI :: Float -> Float -> String
classifyBMI weight height
    | bmi < 18.5 = "underweight"
    | bmi >= 18.5 && bmi <= 25 = "normal weight"
    | bmi >= 25 && bmi <= 30 = "overweight"
    | bmi >= 30 = "obese"
    | otherwise = "invalid"
    where bmi = weight / (height^2)



-- 2.3

{-
max , min :: Ord a => a -> a -> a 
max x y = if x >= y then x else y
min x y = if x <= y then x else y
-}

max3 :: Ord a => a -> a -> a -> a
max3 x y z 
    | x >= y && x >= z = x
    | y >= z = y
    | otherwise = z 
    
min3 :: Ord a => a -> a -> a -> a
min3 x y z 
    | x <= y && x <= z = x
    | y <= z = y
    | otherwise = z 


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

short :: [a] -> Bool
short xs 
    | len < 3 = True
    | otherwise = False
    where len = length xs

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
median2 x y z = x + y + z - (max3 x y z) - (min3 x y z)

prop_median_equiv :: Int -> Int -> Int -> Property
prop_median_equiv x y z = 
        median1 x y z  === median2 x y z 

-- 2.8

propDivs :: Integer -> [Integer]
propDivs n = [ d |d <- [1 .. n-1], n `mod` d == 0]

-- 2.9 

perfects :: Integer -> [Integer]
perfects limit = [n | n <-[1..limit], sum(propDivs n) == n]

-- 2.10

pyths :: Integer -> [(Integer,Integer,Integer)]
pyths limit = [(x,y,z) | x <- [1..limit], y <- [1..limit], z <- [1..limit], x^2 + y^2 == z^2]


-- 2.11

isPrime :: Integer -> Bool
isPrime n = length [d | d <- [1..n], n `mod` d == 0] == 2 

