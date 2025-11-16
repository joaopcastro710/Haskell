{- Worksheet1 -}

-- 1.2
-- using length, take, drop

leftHalf :: [a] -> [a]
leftHalf x = take len x
            where len = (length (x) `div` 2)

rightHalf :: [a] -> [a]
rightHalf x = drop len x
            where len = (length (x) `div` 2)

-- 1.3
-- using head, tail, length, take, drop and reverse


second :: [a] -> a 
-- second (x:xs:xss) = xs
second x = head (drop 1 x)


lastt :: [a] -> a
lastt x = head (reverse x)

initt :: [a] -> [a]
initt x = reverse (drop 1 (reverse x))

middle :: [Int] -> Int
middle x = head (drop len x)
        where len = length x `div` 2 

checkPalindrome :: Eq a => [a] -> Bool
checkPalindrome x = x == reverse x

-- 1.4

checkTriangle :: Float -> Float -> Float -> Bool
checkTriangle a b c = (a<b+c) && (b<a+c) && (c<a+b)

-- 1.5

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = sqrt(s*(s-a)*(s-b)*(s-c))
    where s = (a+b+c)/2