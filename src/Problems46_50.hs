{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}
module Problems46_50 (
  table,
  tablen
) where
import           Control.Monad (replicateM)

newtype TruthRow = TR [Bool]

instance Show TruthRow where
  show :: TruthRow -> String
  show (TR bools) = unwords $ map show bools

newtype TruthTable = TT [TruthRow]

instance Show TruthTable where
  show :: TruthTable -> String
  show (TT rows) = unlines $ map show rows

table :: (Bool -> Bool -> Bool) -> TruthTable
table f = tablen 2 (\case [x, y] -> f x y; _ -> undefined)

tablen :: Int -> ([Bool] -> Bool) -> TruthTable
tablen n f = TT $ map (\comb -> TR (comb ++ [f comb])) combs
  where
    combs = replicateM n [True, False]

