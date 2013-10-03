--Problem 11
--Modified run-length encoding.
--Modify the result of problem 10 in such a way that if an element
--has no duplicates it is simply copied into the result list. Only
--elements with duplicates are transferred as (N E) lists.

        --Code from P10

pack2 :: (Eq a) => [a] -> [[a]]
pack2 xs@(x:xxs) = let (xlist, rest) = span (== x) xs in
  case rest of
    [] -> [xlist]
    yx -> xlist:(pack2 rest)

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) $ pack2 xs

        ---------

data Encoded a = Multiple Int a | Single a deriving Show

encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified xs = let preencoded = encode xs in
  map (\(count, x) -> case count of
          1 -> Single x
          y -> Multiple y x) preencoded
  

--Problem 12
--Decode a run-length encoded list.

decodeModified :: (Eq a) => [Encoded a] -> [a]
decodeModified xs = concat $ map helper xs
                    where helper (Multiple c x) = (replicate c x)
                          helper (Single x) = [x]
                            
                            

--Problem 13
--Run-length encoding of a list (direct solution).
--Implement the so-called run-length encoding data compression method
--directly. I.e. don't explicitly create the sublists containing the
--duplicates, as in problem 9, but only count them. As in problem P11,
--simplify the result list by replacing the singleton lists (1 X) by X.

        -- (I like my solution better than those offered on the site.)
encodeDirect :: (Eq a) => [a] -> [Encoded a]
encodeDirect xs = foldr helper [(Single (last xs))] (init xs)
                  where helper x (a:acc) = case a of
                          (Single y) -> if x == y
                                        then (Multiple 2 y):acc
                                        else (Single x):(Single y):acc
                          (Multiple c y) -> if x == y
                                            then (Multiple (c + 1) y):acc
                                            else (Single x):(Multiple c y):acc
                                                         

--Problem 14
--Duplicate the elements of a list.

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)


        -- (From the site, but I liked it)
        -- or, using silliness:
dupli2 = foldr (\x -> (x:) . (x:)) []


--Problem 15
--Replicate the elements of a list a given number of times.

repli :: [a] -> Int -> [a]
repli [] n = []
repli (x:xs) n = (replicate n x) ++ (repli xs n)

         -- (From the site, but I liked it)
         -- or, using the list monad:

repli2 :: [a] -> Int -> [a]
repli2 xs n = xs >>= replicate n


--Problem 16
--Drop every N'th element from a list.

dropEvery :: [a] -> Int -> [a]
dropEvery [] n = []
dropEvery xs n = (take (n - 1) xs) ++ (dropEvery (drop n xs) n)



--Problem 17
--Split a list into two parts; the length of the first part is given.

split :: [a] -> Int -> ([a],[a])
split xs n = (take n xs, drop n xs)
