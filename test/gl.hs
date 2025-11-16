-- IN-10

testPh :: (Ord a, Floating a) => a -> [Char]
testPh ah 
    | ph <= 7 = "acid"
    | ph == 7 = "neutral"
    | otherwise = "basic"
    where ph = -logBase 10 ah


-- IN-11

max3 :: Ord a => a -> a -> a -> a
max3 x y z 
    | x > y && x > z = x
    | y > z = y
    | otherwise = z


-- IN-14

factorial :: (Ord p, Num p) => p -> p
factorial 0 = 1
factorial n 
    | n > 0 = n * factorial (n-1)
    | n < 0 = error "negative value"

-- IN-15

myGcd :: Integral a => a -> a -> a
myGcd a b = myGcdAux a b
    where myGcdAux a 0 = abs a
          myGcdAux a b = myGcdAux b (mod a b) 

-- IN-16

mPower :: (Fractional a, Integral t) => a -> t -> a 
mPower m n 
    | n == 0 = 1
    | n > 0 = m * mPower m (n-1)

-- IN-17 

fib :: (Num a, Ord a , Num p) => a -> p
fib 0 = 0
fib 1 = 1 
fib n 
    | n > 0 = fib(n-2) + fib(n-1)
    | otherwise = error "negative"

-- IN-18

ackermann :: (Num a, Ord a, Num t, Ord t) => a -> t -> t 
ackermann m n
    | m == 0 = n+1
    | (m>0 && n==0) = ackermann (m-1) 1
    | (m>0 && n > 0) = ackermann (m-1) (ackermann m (n-1))
    | otherwise = error "negative"

-- IN-19

pascal :: (Num a, Ord a, Num p) => a -> a -> p
pascal 0 _ = 1
pascal k n 
    | k > 0 && k == n = 1
    | k > 0 && n > k = pascal (k-1) (n-1) + pascal k (n-1)
    | k > 0 && n < k = error "error"
    | otherwise = error "errorr"