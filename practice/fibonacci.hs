import Data.List
import Data.Char

fib :: (Integral a) => a -> [a]
fib 0 = [0]
fib 1 = [1,0]
fib n = (x + y):xs
      where xs@(x:y:ys) = fib (n - 1)


maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list."
maximum' [x] = x
maximum' (x:xs)
         | x > maxTail = x
         | otherwise = maxTail
         where maxTail = maximum' xs

wordlist :: String -> [String]
wordlist "" = []
wordlist xs = first : wordlist rest
              where words = span (/= ' ') xs
                    first = fst words
                    rest = drop 1 (snd words)

substrings :: Int -> String -> [String]
substrings 0 _ = [""]
substrings _ "" = [""]
substrings n xs = map (take n) (filter (\xs -> length xs >= n)
                                       (tails xs))

search :: String -> String -> Bool
search "" _ = True
search _ "" = False
search xs ys = elem xs ((substrings (length xs)) ys)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = lesser ++ [x] ++ greater
                  where splited = partition (< x) xs
                        lesser = quicksort (fst splited)
                        greater = quicksort (snd splited)

hexToInt :: String -> Int
hexToInt xs = foldl1 (\x y -> x * 16 + y) ints
              where ints = map digitToInt xs

hexStringToInt :: String -> Int
hexStringToInt xs
               | all isHexDigit xs = hexListToInt ints 0
               | otherwise = error "Not a valid hex string."
               where ints = reverse $ map digitToInt xs

hexListToInt :: (Num a) => [a] -> Int -> a
hexListToInt [] _ = 0
hexListToInt (x:xs) power = x * (16^power) + (hexListToInt xs (power + 1))

caesarEncode :: Int -> String -> String
caesarEncode shift xs = let ords = map ord xs
                            shifted = map (+ shift) ords
                            in map chr shifted

caesarDecode :: Int -> String -> String
caesarDecode shift xs = let ords = map ord xs
                            shifted = map (\x -> x - shift) ords
                            in map chr shifted

(!) :: (Num a) => a -> a
! x = x * (!(x - 1))
