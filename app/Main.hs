{-# LANGUAGE OverloadedStrings, DeriveGeneric, BlockArguments #-}
module Main where

import Data.Aeson
import GHC.Generics
import Data.Text (Text, unpack)
import Text.Read
import Data.List
import Data.Maybe
import qualified Data.Map as Map

import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

data Product =
  Product {
      productId :: Int
    , name      :: !Text
    , quantity  :: Int
    , category  :: !Text
    , price     :: Float
  } deriving (Show,Generic)

instance FromJSON Product where
 parseJSON = withObject "Product" $ \object -> do
    id            <- object .: "id"
    name          <- object .: "name"
    quantity      <- object .: "quantity"
    price         <- object .: "price"
    category      <- object .: "category"

    -- Attempt to parse the price string into a float, fail if it can't parse
    parsedPrice <- case toFloat price of
      Just price' -> return price'
      Nothing     -> fail $ "Unexpected invalid float value for price: " <> unpack price

    return Product{
      productId  = id,
      name       = name,
      quantity   = quantity,
      price      = parsedPrice,
      category   = category
    }


data Cart =
  Cart {
      cartId :: Int
    , cartProducts      :: [CartProduct]
  } deriving (Show,Generic)

instance FromJSON Cart where
 parseJSON = withObject "Cart" $ \object -> do
    cartId            <- object .: "id"
    cartProducts      <- object .: "products"

    return Cart{
      cartId       = cartId,
      cartProducts = cartProducts
    }

data CartProduct =
  CartProduct {
      cartProductId :: Int
    , cartProductQuantity  :: Float
  } deriving (Show,Generic)

instance FromJSON CartProduct where
 parseJSON = withObject "CartProduct" $ \object -> do
    cartProductId            <- object .: "id"
    cartProductQuantity      <- object .: "quantity"

    return CartProduct{
      cartProductId       = cartProductId,
      cartProductQuantity = cartProductQuantity
    }

productsUrl :: String
productsUrl = "https://run.mocky.io/v3/87beb2be-15bb-4b42-99b5-d826daaf8689"

getProductsJson :: IO B.ByteString
getProductsJson = simpleHttp productsUrl

cartsUrl :: String
cartsUrl = "https://run.mocky.io/v3/6b59c8b9-deb9-49b7-b9ee-bc591a5ecf6d"

getCartsJson :: IO B.ByteString
getCartsJson = simpleHttp cartsUrl

toFloat :: Text -> Maybe Float
toFloat text =
  Text.Read.readMaybe str'
  where
    str = Data.Text.unpack text
    str' = case str of
      '+' : rest -> rest
      '.' : rest -> '0' : '.' : rest
      other -> other

mostExpensiveProduct :: [Product] -> Product
mostExpensiveProduct products = last $ sortOn price products

categories :: [Product] -> [[Product]]
categories products = groupBy (\x y -> category x == category y) sorted
  where sorted = sortOn category products

-- Return an array of tuples of (category name, number of items in that category)
countCategories :: [[Product]] -> [(Text, Int)]
countCategories = map (\group -> (groupToCategoryName group, length group))
  where groupToCategoryName g = category $ head g

sortCountedCategories :: [(Text, Int)] -> [(Text, Int)]
sortCountedCategories = sortOn extractCount

extractCount :: Num b => (a, b) -> b
extractCount (_, count) = count

type ProductMap = Map.Map Int Product

buildProductMap :: [Product] -> ProductMap
buildProductMap products = Map.fromList $ map productToTuple products
  where productToTuple p = (productId p, p)

cartValue :: ProductMap -> Cart -> (Int, Float)
cartValue productMap cart = (cartId cart, sum $ map calculateTotalPrice $ cartProducts cart)
  where lookupProductPrice productId = case Map.lookup productId productMap of
          Just p -> price p
          _      -> error ("Couldn't find product" ++ show productId)
        calculateTotalPrice cartProduct = lookupProductPrice (cartProductId cartProduct) * cartProductQuantity cartProduct

main :: IO ()
main = do
  productData <- fmap eitherDecode getProductsJson :: IO (Either String [Product])
  cartData <- fmap eitherDecode getCartsJson       :: IO (Either String [Cart])

  case (productData, cartData) of
    (Right ps, Right cartData) -> do
      let mostExpensive        = mostExpensiveProduct ps
      let categoryGroupCounts  = sortCountedCategories $ countCategories $ categories ps
      let productMap           = buildProductMap ps
      let cartValues           = sortOn extractCount $ map (cartValue productMap) cartData
      let mostExpensiveCart    = last cartValues
      print $ "Most products = "          ++ show (last categoryGroupCounts)
      print $ "Most expensive product = " ++ show mostExpensive
      print $ "Highest value cart = "     ++ show mostExpensiveCart

    (Left err, _) -> putStrLn err
    (_, Left err) -> putStrLn err
        

      

  
