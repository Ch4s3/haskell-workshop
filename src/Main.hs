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
-- Maybe: never neither, never both

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

safeHead :: [a] -> Maybe a
safeHead xs = 
  case(xs) of
    [] -> Nothing
    (x:xs) -> Just x

isAnagram :: String -> String -> Bool
isAnagram xs ys = sort(xs) == sort(ys)

main :: IO ()
main = do
  putStrLn "hello world"