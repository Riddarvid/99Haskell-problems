module Problems21_28 (
  insertAt,
  range
) where

insertAt :: a -> [a] -> Int -> Maybe [a]
insertAt x xs 1       = Just (x : xs)
insertAt _ [] _       = Nothing
insertAt x (y : ys) n = (y :) <$> insertAt x ys (n - 1)

range :: (Enum a, Ord a) => a -> a -> [a]
range m n
  | m <= n = m : range (succ m) n
  | otherwise = []


