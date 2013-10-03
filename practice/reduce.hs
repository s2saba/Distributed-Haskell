
reduce :: (a -> a -> a) -> [a] -> Maybe a
reduce f [] = Nothing
reduce f [x] = Just x
reduce f [x,y] = Just (f x y)
reduce f (x:y:xs) = reduce f ((f x y):xs)
