{-# LANGUAGE OverloadedStrings #-}
-- ↑ByteStringとして文字列リテラルを扱う
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 as B
import Data.Map as Map
import Data.HashMap.Strict as HM
import Data.Text as T
import Control.Applicative
import Control.Monad

-- single type json
s = B.pack "{\"name\": \"taro\", \"age\": \"20\"}"
m = decode s :: Maybe (Map String String)
n = m >>= Map.lookup "name"

-- multiple type json
mjsobj :: Maybe Object
mjsobj = decode $ B.pack "{\"name\":\"Dave\",\"age\":2}"
mage :: Maybe Integer
mage = mjsobj >>= parseMaybe (.: (T.pack "age"))
mname :: Maybe String
mname = mjsobj >>= parseMaybe (.: (T.pack "name"))

-- map to custom type
data Person = Person {name :: String, age :: Integer} deriving (Show)
instance FromJSON Person where
     parseJSON (Object v) = Person <$>
                            v .: "name" <*>
                            v .: "age"
     parseJSON _          = mzero

p :: Maybe Person
p2 :: Maybe Person
p = decode "{\"name\":\"Joe\",\"age\":12}"
p2 = decode ""
