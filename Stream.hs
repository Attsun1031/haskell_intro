import Control.Monad
import Data.Char
import Data.List
import System.IO
import Control.Exception
import System.Environment
import System.Directory
import qualified Data.ByteString.Lazy as B

--main = interact res

res :: String -> String
res = unlines . map (\xs -> if isPal xs then "palindrom" else "not a palindrom") . lines

isPal :: String -> Bool
isPal xs = xs == reverse xs


--main = do
--    handle <- openFile "haiku.txt" ReadMode
--    contents <- hGetContents handle
--    putStr contents
--    hClose handle
--main = do
--    withFile "haiku.txt" ReadMode $ \handle -> do
--        contents <- hGetContents handle
--        putStr contents

-- TODOリスト
-- 追加
--main = do
--    todoItem <- getLine
--    appendFile "todo.txt" (todoItem ++ "\n")

-- コマンドライン引数
{-
main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "the arguments are:"
    mapM_ putStrLn args
    putStrLn "the progname is:"
    putStrLn progName
-}

{-
add :: [String] -> IO ()
add args = do
    putStrLn "add action"
    mapM_ putStrLn args

view args = do
    putStrLn "view action"
    mapM_ putStrLn args

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch _ = \_ -> do
    putStrLn "invalid"
-}

--main = do
--    (command:args) <- getArgs
--    dispatch command args

copy source dest = do
    contents <- B.readFile source
    bracketOnError
        (openTempFile "." "tmp")
        (\(tmpName, tmpHandle) -> do
            hClose tmpHandle
            removeFile tmpName)
        (\(tmpName, tmpHandle) -> do
            B.hPutStr tmpHandle contents
            hClose tmpHandle
            renameFile tmpName dest)

main = do
    (source:dest:_) <- getArgs
    copy source dest
