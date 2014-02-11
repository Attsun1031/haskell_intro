import Data.List
import Data.Ratio
import Control.Monad

-- Prob
newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show
instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

flattern :: Prob (Prob a) -> Prob a
flattern (Prob xs) = Prob $ concat $ map multAll xs
  where multAll (Prob innerxs, p) = map (\(x, r) -> (x, r*p)) innerxs

instance Monad Prob where
  return x = Prob [(x, 1%1)]
  m >>= f = flattern (fmap f m)
  fail _ = Prob []
