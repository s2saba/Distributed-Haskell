import Control.Monad.State

type Stack a = [a]

push :: a -> State (Stack a) ()
push a = state $ \xs -> ((), (a:xs))

pop :: State (Stack a) a
pop = state $ \(x:xs) -> (x, xs)


testfunc :: (Monad m, Num a) => m a -> m a
testfunc m = do
  x <- m
  return (x + 1)
  return (x + 2)
  return (x + 3)
  
testfunc2 :: (Monad m, Num a) => m a -> m a
testfunc2 m = m >>= (\x -> return (x + 1) >> return (x + 2) >> return (x + 3))

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing
