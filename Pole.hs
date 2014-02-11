-- Monad の鍛錬

-- 綱渡り（Maybe 版）
type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

routine = do
  start <- return (0, 0)
  first <- landLeft 2 start
  second <- landRight 2 first
  landLeft 1 second


-- 綱渡り（Either版）
landLeft' :: Birds -> Pole -> Either String Pole
landLeft' n (left, right)
  | abs ((left + n) - right) < 4 = Right (left + n, right)
  | otherwise = Left ("left: " ++ show (left + n) ++ ", right: " ++ show right)

landRight' :: Birds -> Pole -> Either String Pole
landRight' n (left, right)
  | abs (left - (right + n)) < 4 = Right (left, right + n)
  | otherwise = Left ("left: " ++ show left ++ ", right: " ++ show (right + n))

routine' = do
  start <- return (0, 0)
  first <- landLeft' 2 start
  second <- landRight' 2 first
  landLeft' 4 second

