module AlgoUtils where

import Algo
import Data.Maybe (isJust, fromJust)
import PortCapability

{-# ANN module "HLint: ignore Use camelCase" #-}

ceil2 :: Int -> Int
ceil2 n = n `div` 2 + n `rem` 2

pc_fn1 :: (Int -> Maybe PortCapability) -> [PortCapability]
pc_fn1 fn1 = let pcs = takeWhile isJust $ map fn1 [1..]
             in if null pcs then [] else [fromJust $ last pcs]

pc_fn2 :: (Int -> Int -> Maybe PortCapability) -> [PortCapability]
pc_fn2 fn2 = let pcs = takeWhile (not.null) $ map (pc_fn1 . fn2) [1..]
             in filter_redundant_pcs $ concat pcs

pc_fn3 :: (Int -> Int -> Int -> Maybe PortCapability) -> [PortCapability]
pc_fn3 fn3 = let pcs = takeWhile (not.null) $ map (pc_fn2 . fn3) [1..]
             in filter_redundant_pcs $ concat pcs

pc_noop :: PortCapability
pc_noop = portCap "NOOP"

pc_nR :: Int -> PortCapability
pc_nR 0 = pc_noop
pc_nR n = portCap $ show n ++ "R"

pc_nW :: Int -> PortCapability
pc_nW 0 = pc_noop
pc_nW n = portCap $ show n ++ "W"

pc_nRW :: Int -> PortCapability
pc_nRW 0 = pc_noop
pc_nRW n = portCap $ show n ++ "RW"

pc_nRor1W :: Int -> PortCapability
pc_nRor1W 0 = pc_nW 1
pc_nRor1W n = portCap $ show n ++ "Ror1W"

pc_nRmW :: Int -> Int -> PortCapability
pc_nRmW 0 0 = pc_noop
pc_nRmW 0 m = pc_nW m
pc_nRmW n 0 = pc_nR n
pc_nRmW n m = portCap $ show n ++ "R" ++ show m ++ "W"

filter_redundant :: (a -> a -> Bool) -> [a] -> [a]
filter_redundant covers_fn = fr []
    where fr goods [] = goods
          fr goods (a:rems)
              | any (`covers_fn` a) $ goods ++ rems = fr goods rems
              | otherwise = fr (a:goods) rems

filter_redundant_pcs :: [PortCapability] -> [PortCapability]
filter_redundant_pcs = filter_redundant covers

alg_fn2 ::   (Int -> Int -> Int) -- level incing fn
          -> (Int -> Bool) -- lvl predicate
          ->  String -- Algo name
          -> (PortCapability -> PortCapability -> [PortCapability]) -- pc fn
          -> (String -> PortCapability -> Int -> [PortCapability] -> Algo) -- algo cons
          ->  Algo -> Algo -> [Algo]
alg_fn2 lvlfn lvlpred alg_name alg_pc_fn alg_cons (Algo _ pc1 lvl1 _ ) (Algo _ pc2 lvl2 _)
    | lvlpred lvl = alg_pc_fn pc1 pc2 >>= (\pc -> [alg_cons alg_name pc lvl [pc1, pc2]])
    | otherwise   = []
    where lvl = lvlfn lvl1 lvl2

alg_fn1 ::   (Int -> Int) -- level incing fn
          -> (Int -> Bool) -- lvl predicate
          ->  String -- Algo name
          -> (PortCapability -> [PortCapability]) -- pc fn
          -> (String -> PortCapability -> Int -> [PortCapability] -> Algo) -- algo cons
          ->  Algo -> [Algo]
alg_fn1 lvlfn lvlpred alg_name alg_pc_fn alg_cons (Algo _ pc1 lvl1 _)
    | lvlpred lvl  = alg_pc_fn pc1 >>= (\pc -> [alg_cons alg_name pc lvl [pc1]])
    | otherwise    = []
    where lvl = lvlfn lvl1

