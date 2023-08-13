{-# LANGUAGE TupleSections #-}
module Problems31_41 (
  isPrime,
  myGCD,
  coprime,
  totient,
  primeFactors,
  primeFactorsMult,
  totientImproved,
  propTot,
  primesR,
  goldbach
) where
import           Data.Foldable   (find)
import           Data.Tuple      (swap)
import           Problems1_10    (encode)
import           Test.QuickCheck (Property, (===), (==>))

isPrime :: Integer -> Bool
isPrime n | n < 2 = False
isPrime 2 = True
isPrime n = all (\d -> n `mod` d /= 0) [2 .. n - 1]

myGCD :: Integer -> Integer -> Integer
myGCD a b = myGCD' (abs a) (abs b)

myGCD' :: Integer -> Integer -> Integer
myGCD' a 0 = a
myGCD' a b = myGCD' b (a `mod` b)

coprime :: Integer -> Integer -> Bool
coprime a b = myGCD a b == 1

totient :: Integer -> Int
totient n = length $ filter (coprime n) [1 .. n]

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = case factor of
  Nothing      -> [n]
  Just factor' -> factor' : primeFactors (n `div` factor')
  where
    factor = find (\f -> n `mod` f == 0) [2 .. ceiling $ sqrt (fromInteger n :: Double)]

primeFactorsMult :: Integer -> [(Integer, Int)]
primeFactorsMult = map swap . encode . primeFactors

totientImproved :: Integer -> Int
totientImproved = product . map (uncurry term) . primeFactorsMult
  where
    term :: Integer -> Int -> Int
    term p m = let p' = fromInteger p in (p' - 1) * p' ^ (m - 1)

propTot :: Integer -> Property
propTot n = n > 0 ==> totient n === totientImproved n

primesR :: Integer -> Integer -> [Integer]
primesR l h = filter isPrime [l .. h]

goldbach :: Integer -> (Integer, Integer)
goldbach n = case find (\x -> isPrime $ n - x) primes of
  Nothing  -> error "Goldbach disproven :)"
  Just res -> (res, n - res)
  where
    primes = primesR 2 n
