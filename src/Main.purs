module Main where

import Prelude

import Data.Array (cons)
import Data.Array (uncons)
import Data.Array.NonEmpty (head)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (overF)
import Effect (Effect)
import Effect.Console (log)

-- https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/
data TrafficLight
  = Red
  | Yellow
  | Green
  | Purple
  | Brown
  | Black

data TwoLights
  = MkTwoLights TrafficLight TrafficLight

twoLights :: TwoLights
--twoLights = MkTwoLights { car : Red,  pedestrian: Green }
twoLights = MkTwoLights Red Green

showLight :: TrafficLight -> String
showLight Red = "rojo"

showLight Yellow = "amarillo"

showLight Green = "verde"

showLight _ = "are you stupid?"

add :: Int -> Int -> Int
add x y = x + y

x :: Int
x = add 1 $ add 2 3

incr :: Int -> Int
incr = f
  where
  f :: Int -> Int
  f = add 1

double :: Int -> Int
double = (*) 2

doubleThenIncr :: Int -> Int
doubleThenIncr = incr <<< double -- incr(double(x)) --- incr ◦ double -- incr . double

--kth 0 [] -> Nothing
--kth 0 [1,2,3,...] -> Just 1

-- kth 0 [1,2,3,4,5] --> Just 1
-- kth 1 [1,2,3,4,5] --> Just 2
-- kth 2 [1,2,3,4,5] --> Just 3

kth :: forall t. Int -> Array t -> Maybe t
kth _ [] = Nothing
kth k list = case uncons list of
  Nothing -> Nothing
  Just { head, tail } -> case k of
    0 -> Just head
    otherwise -> kth (k-1) tail

-- take 3 [1,2,3,4,5] -> [1,2,3]
-- take 3 [1,2] -> [1,2]
take :: forall t. Int -> Array t -> Array t
take 0 list = []
take n list = case uncons list of
  Nothing -> []
  Just { head, tail } -> cons head $ take (n-1) tail

-- drop 3 [1,2,3,4,5] -> [4,5]
-- drop 3 [1,2] -> []
drop :: forall t. Int -> Array t -> Array t
drop 0 list = list
drop n list = case uncons list of
  Nothing -> []
  Just { head, tail } -> drop (n-1) tail

last :: forall t. Array t -> Maybe t
last list = case uncons list of
  Nothing -> Nothing
  Just { head, tail: [] } -> Just head
  Just { head, tail } -> last tail

length :: forall t. Array t -> Int
length list = case uncons list of
  Nothing -> 0
  Just { head, tail } -> 1 + length tail

-- snoc [] 1 -> [1]
-- snoc [2] 1 -> [2,1]
-- snoc [3,2] 1 -> [3,2,1]

snoc :: forall t. Array t -> t -> Array t
snoc list elem = case uncons list of
  Nothing -> [elem]
  Just { head, tail } -> cons head (snoc tail elem)

reverse :: forall t. Array t -> Array t
reverse list = case uncons list of
  Nothing -> []
  Just { head, tail } -> snoc (reverse tail) head

type Person
  = { age :: Int
    , city :: String
    , name :: String
    }

person :: Person
person = { name: "John", age: 31, city: "New York" }


main :: Effect Unit
main = do
  log $ showLight Purple
  log (show (incr 10))
  log $ fromMaybe "Empty list" $ last $ [ "First", "Second", "Third" ]
  log $ fromMaybe "Empty list" $ last $ []
  log $ fromMaybe "Not found" $ kth 3 [1,2,3,4,5,6] <#> show
  log $ fromMaybe "Not found" $ map show $ kth 3 [1,2,3,4,5,6]
  log $ show $ take 3 [1,2,3,4,5,6] <#> show
  log $ show $ drop 3 [1,2,3,4,5,6] <#> show
  log $ show $ reverse [1,2,3,4,5,6] <#> show
  log $ show $ snoc [1,2,3,4,5,6] 7 <#> show
  log $ show $ length [1,2,3,4,5,6]
  log $ show $ incr 10 -- mathematical way compose
  incr 10 # show # log -- human readble way andthen
  log "🍝"
