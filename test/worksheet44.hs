import Data.Char (ord, chr, isLower, isUpper)
{- Worksheet 4-}

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


-- Programming with IO

-- 4.5
rot13Char :: Char -> Char
rot13Char c
  | isLower c = chr (base 'a')
  | isUpper c = chr (base 'A')
  | otherwise = c
    where 
        base a = ord a + ((ord c - ord a + 13) `mod` 26)

rot13 :: String -> String
rot13 = map rot13Char

main :: IO ()
main = do 
  txt <- getContents
  putStr (rot13 txt)


prop_rot13Involutive :: String -> Bool
prop_rot13Involutive s = rot13 (rot13 s) == s



-- 4.6

type AWord     = String
type Line      = [AWord]
type Paragraph = [Line]

fillWords :: Int -> [AWord] -> Paragraph
fillWords width = go [] [] 0
  where
    -- go paragrafo linhaAtual comprimentoAtual restoDasPalavras
    go :: Paragraph -> Line -> Int -> [AWord] -> Paragraph
    go para currLine currLen [] =
      if null currLine
        then reverse para
        else reverse (currLine : para)
    go para currLine currLen (w:ws)
      | null currLine =
          -- linha vazia: começamos com esta palavra
          go para [w] (length w) ws
      | currLen + 1 + length w <= width =
          -- cabe na linha atual (1 é o espaço)
          go para (currLine ++ [w]) (currLen + 1 + length w) ws
      | otherwise =
          -- não cabe: fecha linha atual e começa nova
          go (currLine : para) [w] (length w) ws
