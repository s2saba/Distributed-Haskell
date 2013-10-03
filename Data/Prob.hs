module Data.Prob (
  Prob (..)
, coin
, die
, xTrial
, eventReduce
, plookup  
, trials
) where

import Data.Ratio
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Map as M

newtype Prob a = Prob { getProb :: Map [a] Rational } deriving (Show, Eq)

instance Functor Prob where
  fmap f (Prob m) = Prob $ mapKeysMonotonic (\x -> fmap f x) m

coin :: Prob Bool
coin = Prob $ fromList [([True], 1%2), ([False], 1%2)]

intCoin :: Prob Int
intCoin = Prob $ fromList [([1], 1%2), ([0], 1%2)]

unfairCoin :: Prob Int
unfairCoin = Prob $ fromList [([1], 1%3), ([0], 2%3)]

die :: Prob Int
die = Prob $ fromList [([1], 1%6),
                       ([2], 1%6),
                       ([3], 1%6),
                       ([4], 1%6),
                       ([5], 1%6),
                       ([6], 1%6)]

xTrial :: (Ord a) => Int -> Prob a -> Prob a
xTrial count prob = trials $ replicate count prob

trials :: (Ord a) => [Prob a] -> Prob a
trials prob = let maps = Prelude.map (\(Prob m) -> m) $ probReduce1 prob in
  Prob $ unions maps
  

probReduce1 :: (Ord a) => [Prob a] -> [Prob a]
probReduce1 ((Prob m):[]) = [Prob m]
probReduce1 ((Prob m1):(Prob m2):maps) =
  let xm = toList m1
      ym = toList m2 in
  do
    (x, px) <- ym
    (y, py) <- xm
    probReduce1 ((Prob $ fromList [(x ++ y, px * py)]):maps)

addKey :: (Ord a) => [a] -> Rational -> State (Map [a] Rational) ()
addKey xs prob = state $ (\m -> case M.lookup xs m of
                             Nothing -> ((), insert xs prob m)
                             Just currProb -> ((), insert xs (prob + currProb) m))

reduceKeys :: (Ord a) => (a -> a -> a) -> [([a], Rational)] -> State (Map [a] Rational) ()
reduceKeys f ((event, prob):[]) = do
  addKey [(foldl1 f event)] prob
reduceKeys f ((event, prob):xs) = do
  addKey [(foldl1 f event)] prob
  reduceKeys f xs
  
eventReduce :: (Ord a) => (a -> a -> a) -> Prob a -> Prob a
eventReduce f (Prob m) =
  let xs = toList m 
      (_, endMap) = (runState $ reduceKeys f xs) M.empty in
  Prob endMap
  
plookup :: (Ord a) => [a] -> Prob a -> ([a], Maybe Rational)
plookup key (Prob m) = (key, M.lookup key m)



interesting0 = xTrial 1 coin

interesting1 = xTrial 2 coin

interesting2 = eventReduce (\x y -> x && y) $ xTrial 2 coin

interesting3 = eventReduce (\x y -> x || y) $ xTrial 3 coin

interesting4 = eventReduce (\x y -> x + y) $ xTrial 2 die

interesting5 = trials [intCoin, die]

interesting6 = trials [intCoin, die, die]

interesting7 = trials [intCoin, eventReduce (\x y -> x + y) $ xTrial 2 die]

interesting8 = trials [interesting4, interesting4]

interesting9 = xTrial 2 interesting4
