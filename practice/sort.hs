
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
  where
    lesser = filter (< p) xs
    greater = filter (>= p) xs

halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve [x] = ([x],[])
halve xs = (firsthalf,secondhalf)
           where firsthalf = (take ((length xs) `div` 2) xs)
                 secondhalf = (drop ((length xs) `div` 2) xs)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs@(x:rxs) ys@(y:rys) 
      | x < y = x : (merge rxs ys)
      | otherwise = y : (merge xs rys)


mergesort :: Ord a => [a] => [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort $ fst half) (mergesort $ snd half)
               where half = halve xs

