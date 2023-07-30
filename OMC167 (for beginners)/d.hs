import Data.List

gmax :: [Integer] -> Integer -> Integer -> Integer
gmax [] nowval nextmul | (mod nowval 11 == 0) = nowval
                       | otherwise = 0
gmax list nowval nextmul = maximum [gmax (delete x list) (nowval+x*nextmul) (div nextmul 10) | x <- list]

fmax :: Integer
fmax = maximum [gmax ([0..a-1]++[a+1..9]) (a*10^9) (10^8) | a <- [1..9]]

gmin :: [Integer] -> Integer -> Integer -> Integer
gmin [] nowval nextmul | (mod nowval 11 == 0) = nowval
                       | otherwise = 9876543210
gmin list nowval nextmul = minimum [gmin (delete x list) (nowval+x*nextmul) (div nextmul 10) | x <- list]

fmin :: Integer
fmin = minimum [gmin ([0..a-1]++[a+1..9]) (a*10^9) (10^8) | a <- [1..9]]

main = print(fmax - fmin)