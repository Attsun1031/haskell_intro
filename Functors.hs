-- アプリカティブ
import Control.Monad
import Control.Monad.Writer
import Control.Applicative

-- IO に対する fmap
{-
main = do line <- fmap reverse getLine
          putStrLn $ "Your input: " ++ line
-}


-- ファンクタ則に従わない型
data CMaybe a = CNothing | CJust Int a deriving (Show)
instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter + 1) (f x)


-- 関数アプリカティブの理解
-- この関数は、引数に (+3) したものと (*100) したものに対して (+) し、5 を適用した時の答えは 508 となる。
f = (+) <$> (+3) <*> (*100)

{-
まず、関数のアプリカティブの定義を確認する。

  instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)

(+) <$> (+3) == (pure x) <*> (+3) なので、前半部分の関数は以下のようになる。

  \x -> ((\_ -> (+)) x (x + 3))

さらに後半部分（(*100)) とこの関数を <*> すると、

  \xx -> (\x -> ((\_ -> (+)) x (x + 3))) xx (xx * 100)

となる。
つまり、前半部分で (x + 3) とを部分適用した (+) 関数を作り、後半でそれに (xx * 100) を適用している。
-}


--
-- モナド
--

-- ナイトの移動
type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <- [(c+2, r-1), (c+2, r+1), (c-2, r-1), (c-2, r+1), (c+1, r-2), (c+1, r-2), (c-1, r-2), (c-1, r+2)]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

-- モナド関数合成版
inN :: Int -> KnightPos -> [KnightPos]
inN n start = return start >>= foldr (<=<) return (replicate n moveKnight)

canReachInN :: Int -> KnightPos -> KnightPos -> Bool
canReachInN n start end = end `elem` inN n start

{-
1. 3打目でdestに行けるか見る
2. 2打目で1を満たす手に行ける手があるか見る
3. 1打目で2を満たす手に行ける手があるか見る
-}
computeRoute :: KnightPos -> KnightPos -> [[KnightPos]]
computeRoute start dest =
  let c3 = canReachIn3 start dest
      c2 = if c3
              then filter (\x -> dest `elem` (moveKnight x)) $ return start >>= moveKnight >>= moveKnight
              else []
      c1 =  filter (\x -> length (filter (\xx -> xx `elem` c2) $ moveKnight x) > 0) $ return start >>= moveKnight
  in concat $ fmap (\r1 ->
    let c2r = filter (\r2all -> r2all `elem` c2) $ moveKnight r1
    in fmap (\r2 -> r1:r2:dest:[]) c2r)
    c1


gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcd' b (a `mod` b)


-- 差分リスト
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

-- 差分リストを浸かった逆向きのgcd'
gcdReverse :: Int -> Int -> Writer (DiffList String) Int
gcdReverse a b
  | b == 0 = do
      tell (toDiffList ["Finished with " ++ show a])
      return a
  | otherwise = do
      result <- gcdReverse b (a `mod` b)
      tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
      return result

-- 通常のリストと差分リストの左結合速度比較
finalCountDown :: Int -> Writer [String] ()
finalCountDown 0 = do
  tell ["0"]
finalCountDown x = do
  finalCountDown (x-1)
  tell [show x]

finalCountDown' :: Int -> Writer (DiffList String) ()
finalCountDown' 0 = do
  tell (toDiffList ["0"])
finalCountDown' x = do
  finalCountDown' (x-1)
  tell (toDiffList [show x])


-- Readerモナド（関数のモナド）
addStuff :: Int -> Int
addStuff = do
  a <- (*2)
  b <- (+10)
  return (a+b)

-- addStuff を do 使わないパターン
addStuff' :: Int -> Int
addStuff' = (\x -> x*2) >>= (\y z -> z + (y+10))
