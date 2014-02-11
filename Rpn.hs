import Control.Monad
import Data.List

-- 逆ポーランド記法
-- solveRPN "2 3 +" -> 7
solveRPN :: String -> Double
solveRPN = head . foldl foldRPN [] . words
  where foldRPN (x:y:acc) "+" = y+x:acc
        foldRPN (x:y:acc) "-" = y-x:acc
        foldRPN (x:y:acc) "*" = y*x:acc
        foldRPN (x:y:acc) "/" = y/x:acc
        foldRPN (x:y:acc) "^" = y**x:acc
        foldRPN acc x = read x:acc

-- monad版
readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x, "")] -> Just x
                                _ -> Nothing

folding :: [Double] -> String -> Maybe [Double]
folding (x:y:acc) "+" = return (y+x:acc)
folding (x:y:acc) "-" = return (y-x:acc)
folding (x:y:acc) "*" = return (y*x:acc)
folding (x:y:acc) "/" = return (y/x:acc)
folding (x:y:acc) "^" = return (y**x:acc)
folding acc x = liftM (:acc) (readMaybe x)

solveRPN' :: String -> Maybe Double
solveRPN' st = do
  [result] <- foldM folding [] (words st)
  return result

