-- 3章 関数
doubleMe x = x + x
doubleUs x y = x * 2 + y * 2

doubleSmallNumber x = (if x > 100 then x else x*2) + 1

removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase st = [ c | c <- st, c `elem` ['A'..'Z']]


-- 4章 再帰
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> t -> [t]
replicate' n t
    | n <= 0 = []
    | otherwise = t : replicate' (n -1) t

take' :: Int -> [t] -> [t]
take' n l
    | n <= 0 = []
    | n > length l = take n' l
    | otherwise = (take' n' l) ++ [l!!n']
  where n' = n - 1

--take' 0 _ = []
--take' n (x:xs) = x : take' (n - 1) xs

zip' :: [a] -> [b] -> [(a, b)]
{-
zip' al bl
    | lenA == 0  || lenB == 0 = []
    | otherwise = (head al, head bl) : zip' (tail al) (tail bl)
  where lenA = length al
        lenB = length bl
-}
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

quicksort :: (Ord e) => [e] -> [e]
quicksort es = let filteres f = filter f es in case es of
    [] -> []
    -- リスト内包表記版
    --es@(p:_) -> quicksort [e | e <- es, e < p] ++ [p] ++ quicksort [e | e <- es, e > p]
    -- 高階関数版
    es@(p:_) -> quicksort (filteres (< p)) ++ [p] ++ quicksort (filteres (> p))


-- 5章 高階関数
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

flip' :: (a -> b -> c) -> b -> a -> c
flip' f b a = f a b


-- コラッツ列の例
cl :: Integer -> [Integer]
cl x
    | x == 1 = [1]
    | even x = x : cl (x `div` 2)
    | otherwise = x : cl (x * 3 + 1)

numLongChains :: Int
numLongChains = length (filter f (map cl [1..100]))
  where f l = length l > 15

-- 合計関数（再帰）
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- 合計関数（fold）
sum'' :: (Num a) => [a] -> a
-- sum'' xs = foldl (\acc x -> acc + x) 0 xs
sum'' xs = foldl (+) 0 xs

-- elem
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

-- maximum''
maximum'' :: (Ord a) => [a] -> a
maximum'' = foldl1 max

-- 自然数の平方根を足していった時に1000を超えるのは幾つ目か
sqSumLen :: Int
sqSumLen = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
sqSum :: Int
sqSum = sum . takeWhile (<1000) . filter odd $ map (^2) [1..]

