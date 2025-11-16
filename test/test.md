# Notes 

## Worksheet 1 

- using `:doc drop`, we can see examples and explanation of what any Prelude function does
- usar o ``div``

## Worksheet 2

- attention to all the list comprehension cases:

```
propDivs :: Integer -> [Integer]
propDivs n = [ d |d <- [1 .. n-1], n `mod` d == 0]

perfects :: Integer -> [Integer]
perfects limit = [n | n <-[1..limit], sum(propDivs n) == n]

pyths :: Integer -> [(Integer,Integer,Integer)]
pyths limit = [(x,y,z) | x <- [1..limit], y <- [1..limit], z <- [1..limit], x^2 + y^2 == z^2]

isPrime :: Integer -> Bool
isPrime n = length [d | d <- [1..n], n `mod` d == 0] == 2 
```