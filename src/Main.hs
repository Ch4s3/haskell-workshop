{-# LANGUAGE TypeApplications #-}

module Main where
import Data.Char
import Data.List (sort)
import Numeric.Natural

absVal x = if (x < 0) then (negate x) else x

-- *Main> :type absVal2
-- absVal2 :: (Ord p, Num p) => p -> p
-- (Ord p, Num p) is a tuple expressing the type
-- constraints of the function absVal2
-- the fat arrow => seperates the constraints from
-- the type signature, the function takes an arg of type p
-- and returns a value of type p (where the type implements Ord and Num)
-- *Main> :type 0
-- 0 :: Num p => p

absVal2 x =
  case (x < 0) of 
    True -> negate x
    False -> x

function :: Integer -> Integer -> Integer
function x y = 
  case (x < y) of
    True -> negate x
    False -> y

-- sum types vs. product types
-- Sum Types
-- Bool is a sum type False | True
-- data Maybe a = Nothing | Just a
-- Maybe: never neither, never bot
-- data Either a b = Left a | Right b

-- Product Types
-- data (->) a b
-- data (,) a b


-- data User is the type constructor
-- = User String Natural is the value constructor
data User = User String Natural deriving (Eq, Show)

newtype Username = Username String deriving (Show)
-- Record syntax
data User2 = User2 { name :: Username
                  , age :: Natural } deriving (Show)

-- data Maybe a = Nothing | Just a

-- *Main> :kind Maybe
-- Maybe :: * -> *

-- *Main> :set -XTypeApplications
-- *Main> :type null
-- null :: Foldable t => t a -> Bool
-- *Main> :type null @[]
-- null @[] :: [a] -> Bool
-- the @ applys a type application

safeHead :: [a] -> Maybe a
safeHead xs = 
  case(xs) of
    [] -> Nothing
    (x:xs) -> Just x

isAnagram :: String -> String -> Bool
isAnagram xs ys = sort(xs) == sort(ys)

checkAnagram :: String -> String -> Bool
checkAnagram xs ys =
  case isWord xs of
    Nothing -> False
    Just xs -> 
      case isWord ys of
        Nothing -> False
        Just ys -> isAnagram xs ys

-- absolute x
--   | x<0 = -x
--   | otherwise = x

-- isWord "" = Nothing
-- isWord xs =
--     case (all isAlpha xs) of
--       False -> Nothing
--       True -> Just xs

isWord :: String -> Maybe String
isWord xs =
  case null xs of
    True -> Nothing
    False -> 
      case (all isAlpha xs) of
        False -> Nothing
        True -> Just xs

main :: IO ()
main = do
  putStrLn "Please enter a word."
  word1 <- getLine 
  putStrLn "Please enter another word."
  word2 <- getLine 
  
  print( checkAnagram word1 word2 ) 