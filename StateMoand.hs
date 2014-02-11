import Control.Monad
import Control.Monad.State

-- State モナド

-- スタック
-- Stateモナドを使わないパターン
type Stack = [Int]
pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

stackMainp :: Stack -> (Int, Stack)
stackMainp stack = let
  ((), newStack1) = push 3 stack
  (a, newStack2) = pop newStack1
  in pop newStack2

-- Stateモナドを使うパターン
pop' :: State Stack Int
pop' = state $ \(x:xs) -> (x, xs)

push' :: Int -> State Stack ()
push' a = state $ \xs -> ((), a:xs)

stackMainp' :: State Stack Int
stackMainp' = do
  push' 3
  pop'
  pop'
-- >>= を使う場合
--stackMainp' = push' 3 >>= \_ -> pop' >>= \_ -> pop'
