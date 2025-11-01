-- 1.2 ------------------------------

leftHalf :: [a] -> [a]
leftHalf xs = take n xs
    where n = length(xs) `div` 2

rightHalf :: [a] -> [a]
rightHalf xs = drop n xs
    where n = length(xs) `div` 2


-- 1.3 ------------------------------

second :: [a] -> a
second xs = head (tail xs)

----

lastt :: [a] -> a
lastt xs = head (reverse xs)

lasttt :: [a] -> a 
lasttt xs = head (drop len xs)
    where len = length(xs) - 1 

----

initt :: [a] -> [a]
initt xs = reverse (drop 1 (reverse xs))

inittt :: [a] -> [a]
inittt xs = reverse (tail (reverse xs))

----

middle :: [a] -> a
middle xs = head (drop l xs)
    where l = length xs `div` 2 

----

checkPalindrome :: Eq a => [a] -> Bool
checkPalindrome xs = xs == reverse xs

-- 1.4 ------------------------------

checkTriangle :: Float -> Float -> Float -> Bool
checkTriangle a b c = (a < b + c) && (b < a + c) && (c < a + b)

-- 1.5 ------------------------------

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = sqrt (s*(s-a)*(s-b)*(s-c))
    where s = (a+b+c)/2


-- 1.6 ------------------------------

{-
a - 3
b - 4
c - 2
d - 1
e - 8
f - 7
g - 10
h - 9
i - 6
j - 5
-}

-- 1.7 ------------------------------

{-
a-erro
b-erro
c-erro
d- erro
e- yes
f- yes, é comparar
g- erro
h-yes
i-yes 
j- erro
k- erro 
l- erro
m- yes
n- erro
o- yes
-}

-- 1.8 ------------------------------

{-

a- second :: [a] -> a 
b- swap :: (a,b) -> (b,a)
c- pair :: a -> (a,a)
d- double :: Num a => a -> a 
e- half :: Fractional a => a -> a 
f- average :: Fractional a => a -> a -> a
g- isLower :: Char -> Bool
h- inRange :: Ord a => a -> a -> a -> Bool
i- isPalindrome :: Eq a => [a] -> Bool
j- twice :: (a -> a) -> a -> a

-}