-- 7章 型
module Shapes
( Point (..)
, Shape (..)
, area
, nudge
, Person (..)
) where

import qualified Data.Map as Map

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float |
             Rectangle Point Point
    deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))


-- レコード構文
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)


data Vector a = Vector a a a deriving (Show)
vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- 型シノニム
type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

-- ロッカーの検索
-- Either を使うと検索に失敗した場合も特定の型を返すことができる（このケースではどちらも String なのであまり恩恵はないが。）
-- Maybe Code とやると、失敗した時の型が Nothing になってしまうので失敗した理由がわからない。
lookupLocker :: Int -> LockerMap -> Either String Code
lookupLocker i m = case Map.lookup i m of
    Nothing -> Left $ "Locker " ++ show i ++ " doesn't exist"
    Just (state, code) -> if state /= Taken
                            then Right code
                            else Left $ "Locker " ++ show i ++ " is already taken"

lockers :: LockerMap
lockers = Map.fromList
    [(100, (Taken, "abc"))
    ,(101, (Free, "def"))
    ,(102, (Taken, "ghi"))]
