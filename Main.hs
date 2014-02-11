-- IO
{-
import Data.Char
main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName

    putStrLn $ "you are " ++ bigFirstName ++ " " ++ bigLastName
    -}

import Control.Monad
main = do
    line <- getLine
    when (not $ null line) $ do
        putStrLn $ reverseWords line
        main
    sequence $ map print ["hoge", "fuga"]
    return ()

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
