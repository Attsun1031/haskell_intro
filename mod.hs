import Data.List (nub)
import Data.Char (ord, chr)
import Geometry.Sphere
import Shapes

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

sEncode :: Int -> String -> String
sEncode n = map (chr . (+n) . ord)
