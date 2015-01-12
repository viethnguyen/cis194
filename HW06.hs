{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HW06 where

import           Data.Aeson
import           Data.Monoid
import           GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

-- Exercise 1
ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool x = x

-- Exercise 2
parseData :: B.ByteString -> Either String Value
parseData bs = fmap ynToBool $ eitherDecode bs

-- Exercise 3
data Person = Person { name :: String, age :: Int}
    deriving (Show, Generic)
instance FromJSON Person

p :: Either String Person
p = eitherDecode "{ \"name\" : \"Richard\", \"age\" : 32 }"

data Market = Market {marketname :: T.Text, x :: Double, y :: Double, state :: T.Text }
    deriving (Show, Generic)
instance FromJSON Market

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets = eitherDecode

-- Exercise 4
loadData :: IO [Market]
loadData = do
    filedata <- B.readFile "markets.json"
    let eitherMrkts = parseMarkets filedata
    case eitherMrkts of Left s -> fail s
                        Right m -> return m

-- Exercise 5
data OrdList a = OrdList {getOrdList :: [a] }
    deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
    mempty = OrdList []
    
    mappend (OrdList x) (OrdList []) = OrdList x
    mappend (OrdList []) (OrdList y) = OrdList y
    mappend (OrdList first@(x:xs)) (OrdList second@(y:ys)) 
         | x < y = OrdList (x : getOrdList (mappend (OrdList xs) (OrdList second)))
         | otherwise = OrdList (y : getOrdList (mappend (OrdList first) (OrdList ys)))

evens :: OrdList Integer
evens = OrdList [2,4,6]

odds :: OrdList Integer
odds = OrdList [1,3,5]

combined :: OrdList Integer
combined = evens <> odds

-- Exercise 6
type Searcher m = T.Text -> [Market] -> m

search :: Monoid m => (Market -> m) -> Searcher m 
search mk_m text mkts = go mkts
    where go [] = mempty
          go (mkt:mkts) = case (text `T.isInfixOf` (marketname mkt)) of 
            True -> mk_m mkt <> go mkts 
            False -> go mkts 

-- Exercise 7 
firstFound :: Searcher (Maybe Market)

