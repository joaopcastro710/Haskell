import Data.Char (ord, chr, isLower, isUpper)
import Data.List (sort)
-- 4.1

calcPi1 :: Int -> Double 
calcPi1 n = sum [numerador / fromIntegral denominador | (numerador,denominador) <- take n(zip(cycle[4,-4])[1,3..])]

calcPi2 :: Int -> Double 
calcPi2 n = 3 + sum [numerador / fromIntegral denominador | (numerador,denominador) <-take n (zip (cycle [4.0, -4.0]) [ (2*k) * (2*k+1) * (2*k+2) | k <- [1..] ])]


-- 4.2

primes :: [Integer]
primes = n [2..]
  where
    n (p:xs) = p : n [ x | x <- xs, x `mod` p /= 0 ]

-- (p, p+2)
twinPrimes :: [(Integer, Integer)]
twinPrimes =
  filter (\(p,q) -> q - p == 2) (zip primes (tail primes))

-- 4.3

{-quando o haskell avalia uma list comprehension com geradores aninhados, ele tenta fixar o primeiro 
gerador e percorrer inteiramente o segundo antes de avançar, e para cada valor do segundo percorre inteiramente
o terceiro. Com [0..] no gerador interior isto nunca termina para i=0 - o gerador j nunca "acaba" para
permitir que i avance. Assim a expressão fica "presa" e não produz a lista desejada.
-}

layer :: Int -> [Integer]
layer n =
  [2^i * 3^j * 5^k | i <- [0..n], j <-[0..n-i], let k = n-i-j]

hamming_layers :: [Integer]
hamming_layers = foldr (merge . sort) [] (map layer [0..])

-- 4.5

rot13char :: Char -> Char
rot13char c
  | isLower c = chr (((ord c - ord 'a' + 13) `mod` 26)+ ord 'a')
  | isUpper c = chr (((ord c - ord 'A' + 13) `mod` 26)+ ord 'A')
  | otherwise = c

rot13 :: String -> String
rot13 = map rot13char

-- 4.6

type AWord = String
type Line = [AWord]
type Paragraph = [Line]

fillWords :: Int -> [AWord] -> Paragraph
fillWords _ [] = []
fillWords w (x:xs) =
    let line = takeWhileFits w (x:xs)
        rest = drop (length line) (x:xs)
    in line : fillWords w rest

-- Auxiliary function: accumulate words while they fit the width
takeWhileFits :: Int -> [AWord] -> [AWord]
takeWhileFits _ [] = []
takeWhileFits w (x:xs) = go (length x) [x] xs
  where
    go curLen line [] = reverse line
    go curLen line (y:ys)
      | curLen + 1 + length y <= w =
          go (curLen + 1 + length y) (y:line) ys
      | otherwise = reverse line
      