--Problem 1
--Find the last element of a list

myLast :: [a] -> a
myLast (x:[]) = x
myLast (_:xs) = myLast xs


--Problem 2
-- Find the last but one element fo a list

myButLast :: [a] -> a
myButLast = head . tail . reverse


--Problem 3
--Find the K'th element of a list. The first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (_:xs) n = elementAt xs (n - 1)

--Problem 4
--Find the number of elements of a list

myLength :: (Integral b) => [a] -> b
myLength xs = recurLength xs 0
  where recurLength [x] n = n + 1
        recurLength (_:xs) n = recurLength xs (n + 1)

        -- This was one from the solutions, but it can't be tail call optimized,
        -- since we must save the stack contexts to increment the count. It will
        -- overflow the stack on large lists. The first is much better, and gets TCO.
myLength2 :: (Integral b) => [a] -> b
myLength2 [] = 0
myLength2 (_:xs) = 1 + (myLength2 xs)


--Problem 5
--Reverse a list

        --First attempt. O(n^2) 
myReverse :: [a] -> [a]
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

         --Next attempt. This on is O(n), but still can't compete in speed with
         --standard library's reverse.
myReverse2 :: [a] -> [a]
myReverse2 (xs) = revreverse xs []
  where revreverse [] ys = ys
        revreverse (x:xs) ys = revreverse xs (x:ys)

        --Final attempt. O(n), faster than the standard library, and uses
        --no memory! (at least in the minimal tests I conducted)
myReverse3 :: [a] -> [a]
myReverse3 xs = foldr (\x acc -> x:acc) [] xs



--Problem 6
--Find out whether a list is a palindrome.

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (myReverse3 xs)


--Problem 7
--Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List lists) = foldr (\x acc ->  (flatten x) ++ acc) [] lists


--Problem 8
--Eliminate consecutive duplicates of list elements.

compress :: (Eq a) => [a] -> [a]
compress xs = foldr (\x acc@(y:ys) -> if y == x then acc else x:acc) [last xs] xs


--Problem 9
--Pack consecutive duplicates of list elements into sublists.
--If a list contains repeated elements they should be placed in separate sublists. 

pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack xs = foldr (\x (a:acc) -> if head a == x then (x:a):acc else [x]:a:acc) [[last xs]] $ init xs 

          --Also good to know is 'span'

pack2 :: (Eq a) => [a] -> [[a]]
pack2 xs@(x:xxs) = let (xlist, rest) = span (== x) xs in
  case rest of
    [] -> [xlist]
    yx -> xlist:(pack2 rest)



--Problem 10
--Run-length encoding of a list. Use the result of problem P09 to implement
--the so-called run-length encoding data compression method. Consecutive
--duplicates of elements are encoded as lists (N E) where N is the number of
--duplicates of the element E.     

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) $ pack2 xs







