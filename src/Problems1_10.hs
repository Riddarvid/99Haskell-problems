module Problems1_10 (
  myLast,
  myButLast,
  elementAt,
  myLength,
  myReverse,
  isPalindrome,
  flatten,
  compress,
  pack,
  encode
) where
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE

-- Problem 1

myLast :: [a] -> Maybe a
myLast []       = Nothing
myLast [x]      = Just x
myLast (_ : xs) = myLast xs

-- Problem 2

myButLast :: [a] -> Maybe a
myButLast []       = Nothing
myButLast [x, _]   = Just x
myButLast (_ : xs) = myButLast xs

-- Problem 3

elementAt :: [a] -> Int -> Maybe a
elementAt _ n | n <= 0 = Nothing
elementAt [] _ = Nothing
elementAt (x : _) 1 = Just x
elementAt (_ : xs) n = elementAt xs (n - 1)

-- Problem 4

myLength :: [a] -> Int
myLength = sum . map (const 1)

-- Problem 5

myReverse :: [a] -> [a]
myReverse []       = []
myReverse (x : xs) = myReverse xs ++ [x]

-- Problem 6

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

-- Problem 7

data NestedList a = NLElem a | NLList [NestedList a]
  deriving Show

flatten :: NestedList a -> [a]
flatten (NLElem x)  = [x]
flatten (NLList xs) = concatMap flatten xs

-- Problem 8

compress :: (Eq a) => [a] -> [a]
compress (x : y : xs)
  | x == y = rest
  | otherwise = x : rest
  where rest = compress (y : xs)
compress xs = xs

-- Problem 9

pack :: (Eq a) => [a] -> [NonEmpty a]
pack [] = []
pack (x : xs) = case packed of
  [] -> [x :| []]
  (ys@(y :| ys') : yss) -> if x == y
    then (x :| y : ys') : yss
    else (x :| []) : ys : yss
  where
    packed = pack xs

-- Problem 10

encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\xs@(x :| _) -> (NE.length xs, x)) . pack
