module Prob where

-- For Rational Numbers. (1%2)
import Data.Ratio
import Control.Applicative
import Control.Monad
import Data.List

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x, p)) xs

instance Monad Prob where
  return x = Prob [(x, 1%1)]
  m >>= f = flatten (fmap f m)  -- Since m >>= f is always join (fmap f m)
                                -- We pick a new way to join probabilities (flatten)
  fail _ = Prob []              -- Fail just like list Monads fail.



flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
  where multAll (Prob innerxs, p) = map (\(x,innerp) -> (x, p * innerp)) innerxs

coin :: Prob [Bool]
coin = Prob [([True], 1%2),([False], 1%2)]

die :: Prob [Int]
die = Prob [([1], 1%6),([2], 1%6),([3], 1%6),([4], 1%6),([5], 1%6),([6], 1%6)]

xTrial :: Int -> Prob [a] -> Prob [a]
xTrial count (Prob prob) = Prob $ probreduce $ replicate count prob

pmult :: ([a], Rational) -> ([a], Rational) -> ([a], Rational)
pmult (xa, pa) (xb, pb) = (xa ++ xb, pa * pb)
                                    
probreduce :: [[([a], Rational)]] -> [([a], Rational)]
probreduce (xs:[]) = xs
probreduce (xs:ys:probs) = do
  x <- xs
  y <- ys
  probreduce ([(pmult x y)]:probs)

eventreduce :: (a -> a -> a) -> Prob [a] -> Prob [a]
eventreduce f (Prob ((ax, prob):[])) = Prob [([(foldl1 f ax)], prob)]
eventreduce f (Prob ((ax, prob):bx)) = let (Prob reducedtail) =
                                             (eventreduce f (Prob bx)) in
                                       Prob (([(foldl1 f ax)], prob):reducedtail)





