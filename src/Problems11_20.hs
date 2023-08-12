module Problems11_20 (
  encodeModified,
  decodeModified,
  encodeDirect,
  dupli,
  repli,
  dropEvery,
  split,
  slice,
  rotate,
  removeAt
) where
import           Problems1_10 (encode)

data RunLength a = Single a | Multiple Int a
  deriving Show

encodeModified :: (Eq a) => [a] -> [RunLength a]
encodeModified = map singlify . encode
  where
    singlify (1, x) = Single x
    singlify (n, x) = Multiple n x

decodeModified :: [RunLength a] -> [a]
decodeModified = concatMap decodeModified'
  where
    decodeModified' (Single x)     = [x]
    decodeModified' (Multiple n x) = replicate n x

encodeDirect :: (Eq a) => [a] -> [RunLength a]
encodeDirect [] = []
encodeDirect (x : xs) = case encoded of
  [] -> [Single x]
  (y : ys) -> case y of
    Single y' -> if x == y' then Multiple 2 x : ys else Single x : y : ys
    Multiple n y' -> if x == y' then Multiple (n + 1) x : ys else Single x : y : ys
  where
    encoded = encodeDirect xs

dupli :: [a] -> [a]
dupli = concatMap (\x -> [x, x])

repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = concatMap (take $ n - 1) $ chunksOf xs
  where
    chunksOf :: [a] -> [[a]]
    chunksOf []  = []
    chunksOf xs' = let (ys, xs'') = splitAt n xs' in ys : chunksOf xs''

split :: [a] -> Int -> Maybe ([a], [a])
split _ n | n < 0 = Nothing
split [] _ = Just ([], [])
split (x : xs) n = do
  (ys, zs) <- split xs (n - 1)
  return (x : ys, zs)

slice :: [a] -> Int -> Int -> [a]
slice xs si ei = take (ei - si') $ drop si' xs
  where
    si' = si - 1

rotate :: [a] -> Int -> [a]
rotate xs n = let (start, end) = splitAt n' xs in end ++ start
  where
    n' = n `mod` length xs

removeAt :: Int -> [a] -> Maybe (a, [a])
removeAt _ [] = Nothing
removeAt 1 (x : xs) = Just (x, xs)
removeAt n (x : xs) = do
  (y, ys) <- removeAt (n - 1) xs
  return (y, x : ys)
