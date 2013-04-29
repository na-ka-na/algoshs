module Algos3 where

import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.List (sort)
import Data.Maybe (fromJust, isJust, maybeToList)
import PortCapability3
import System.Environment

{-# ANN module "HLint: ignore Use camelCase" #-}
----------------------------------------------------------------------------------------------------

ceil2 :: Int -> Int
ceil2 n = n `div` 2 + n `rem` 2

pc_nRW :: Int -> PortCapability
pc_nRW n = portCap $ show n ++ "RW"
pc_nRor1W :: Int -> PortCapability
pc_nRor1W n = portCap $ show n ++ "Ror1W"
pc_nRmW :: Int -> Int -> PortCapability
pc_nRmW n m = portCap $ show n ++ "R" ++ show m ++ "W"

coverss :: [PortCapability] -> [PortCapability]
coverss pcs = coverss' pcs []
              where coverss' [] cs = cs
                    coverss' (x:xs) cs
                        | any (`covers` x) xs = coverss' xs cs
                        | otherwise = coverss' xs (x:cs)
----------------------------------------------------------------------------------------------------

-- MT pc1 pc2 where pc1 >= nRW and pc2 >= 2nR(n/2)W => nRnW
_MT_fn1 :: PortCapability -> PortCapability -> Int -> Maybe PortCapability
_MT_fn1 pc1 pc2 n
    | (pc1 `covers` bank) && (pc2 `covers` state) = Just algo
    | otherwise = Nothing
    where bank  = pc_nRW n
          state = pc_nRmW (2*n) (ceil2 n)
          algo  = pc_nRmW n n

-- MT pc1 pc2 where pc1 >= nRor1W and pc2 >= (n+m)RmW => nRmW
_MT_fn2 :: PortCapability -> PortCapability -> Int -> Int -> Maybe PortCapability
_MT_fn2 pc1 pc2 n m
    | (pc1 `covers` bank) && (pc2 `covers` state) = Just algo
    | otherwise = Nothing
    where bank = pc_nRor1W n
          state = pc_nRmW (n+m) m
          algo = pc_nRmW n m

_MT1 :: PortCapability -> PortCapability -> Maybe PortCapability
_MT1 pc1 pc2 = let pcs = takeWhile isJust $ map (_MT_fn1 pc1 pc2) [1 ..]
              in if null pcs then Nothing else last pcs

_MT2_w :: PortCapability -> PortCapability -> Int -> Maybe PortCapability
_MT2_w pc1 pc2 m = let pcs = takeWhile isJust $ map (\n -> _MT_fn2 pc1 pc2 n m) [1..]
                   in if null pcs then Nothing else last pcs 

-- 3Ror1W 5R3W => 3R2W .. 2R3W
_MT2 :: PortCapability -> PortCapability -> [PortCapability]
_MT2 pc1 pc2 = let pcs = takeWhile isJust $ map (_MT2_w pc1 pc2) [1..]
               in coverss $ map fromJust pcs

_MT :: PortCapability -> PortCapability -> [PortCapability]
_MT pc1 pc2 = let pc' = _MT1 pc1 pc2
                  pcs = _MT2 pc1 pc2
              in coverss $ maybeToList pc' ++ pcs
----------------------------------------------------------------------------------------------------

-- REP pc1 pc2 where pc1 >= nRmW and pc2 >= n'RmW => (n+n')RmW
_REP_fn :: PortCapability -> PortCapability -> Int -> Int -> Int -> Maybe PortCapability
_REP_fn pc1 pc2 n n' m
    | (pc1 `covers` bank1) && (pc2 `covers` bank2) = Just algo
    | otherwise = Nothing
    where bank1 = pc_nRmW n m
          bank2 = pc_nRmW n' m
          algo = pc_nRmW (n+n') m

_REP_w :: PortCapability -> PortCapability -> Int -> Maybe PortCapability
_REP_w pc1 pc2 m = let ns  = takeWhile (\n -> pc1 `covers` pc_nRmW n m) [1..]
                       ns' = takeWhile (\n -> pc2 `covers` pc_nRmW n m) [1..]
                   in if null ns || null ns'
                        then Nothing else _REP_fn pc1 pc2 (last ns) (last ns') m

_REP :: PortCapability -> PortCapability -> [PortCapability]
_REP pc1 pc2 = let pcs = takeWhile isJust $ map (_REP_w pc1 pc2) [1..]
               in if null pcs then [] else [fromJust $ last pcs]

----------------------------------------------------------------------------------------------------

_MT_name :: String; _MT_name = "_MT"
_RL_name :: String; _RL_name = "_RL"
_XR_name :: String; _XR_name = "_XR"
_REP_name :: String; _REP_name = "_REP"
_BASE_name :: String; _BASE_name = "_BASE"

_MAX_MT_LEVEL :: Int; _MAX_MT_LEVEL = 4
_MAX_RL_LEVEL :: Int; _MAX_RL_LEVEL = 4
_MAX_XR_LEVEL :: Int; _MAX_XR_LEVEL = 3

data Algo = Algo String PortCapability Int [Algo] deriving (Eq, Ord)

instance Hashable Algo where
    hashWithSalt salt (Algo name pc lvl algs)
        = foldr hashWithSalt salt $ [hash name, hash pc, lvl] ++ map hash algs

instance Show Algo where
    show (Algo name pc _ algs)
        = show pc ++ (if name==_BASE_name then "" else name)
          ++ (if null algs then "" else "::" ++ show algs)

_Alg2 :: (Int -> Int -> Int) -> (Int -> Bool) -> String
          -> (PortCapability -> PortCapability -> [PortCapability])
          -> (String -> PortCapability -> Int -> [Algo] -> Algo)
          -> Algo -> Algo -> [Algo]
_Alg2 lvlfn lvlpred alg_name alg_pc_fn alg_cons alg1@(Algo _ pc1 lvl1 _ ) alg2@(Algo _ pc2 lvl2 _)
    | lvlpred lvl = alg_pc_fn pc1 pc2 >>= (\pc -> [alg_cons alg_name pc lvl [alg1, alg2]])
    | otherwise   = []
    where lvl = lvlfn lvl1 lvl2

_Alg1 :: (Int -> Int) -> (Int -> Bool) -> String
         -> (PortCapability -> [PortCapability])
         -> (String -> PortCapability -> Int -> [Algo] -> Algo)
         -> Algo -> [Algo]
_Alg1 lvlfn lvlpred alg_name alg_pc_fn alg_cons alg1@(Algo _ pc1 lvl1 _)
    | lvlpred lvl  = alg_pc_fn pc1 >>= (\pc -> [alg_cons alg_name pc lvl [alg1]])
    | otherwise    = []
    where lvl = lvlfn lvl1

_MT_Alg :: Algo -> Algo -> [Algo]
_MT_Alg = _Alg2 (\lvl1 lvl2 -> max lvl1 lvl2 + 1) (<=_MAX_MT_LEVEL) _MT_name _MT Algo

_REP_Alg :: Algo -> Algo -> [Algo]
_REP_Alg = _Alg2 max (const True) _REP_name _REP (\name pc _ _ -> Algo name pc 0 [])

_base_pcs :: [PortCapability]
_base_pcs = [pc_nRW 1, pc_nRmW 1 1, pc_nRW 2, pc_nRmW 2 2]

_base_algs :: [Algo]
_base_algs = map (\pc -> Algo _BASE_name pc 0 []) _base_pcs

_ALG_fns2 :: [Algo -> Algo -> [Algo]]
_ALG_fns2 = [_MT_Alg, _REP_Alg]

_ALG_fns1 :: [Algo -> [Algo]]
_ALG_fns1 = []

_alg_covers :: Algo -> Algo -> Bool
(Algo name1 pc1 lvl1 as1) `_alg_covers` (Algo name2 pc2 lvl2 as2)
    | (name1 == name2) && (pc1 == pc2) && (lvl1 <= lvl2)
        = let [pcs1, pcs2] = map (map (\(Algo _ pc _ _) -> pc)) [as1, as2]
          in and $ zipWith covers pcs2 pcs1
    | otherwise = False

_filter_redundant_algs :: [Algo] -> [Algo]
_filter_redundant_algs = fra []
    where fra good_algs [] = good_algs
          fra good_algs (alg:rem_algs)
              | any (`_alg_covers` alg) $ good_algs ++ rem_algs = fra good_algs rem_algs
              | otherwise = fra (alg:good_algs) rem_algs

_iter_alg :: [Algo] -> Int -> [Algo]
_iter_alg algs n = foldr _iter algs [1..n]
    where _iter _ acc = let as2 = [fn a1 a2 | a1 <- acc, a2 <- acc, fn <- _ALG_fns2]
                            as1 = [fn a | a <- acc, fn <- _ALG_fns1]
                        in _filter_redundant_algs $ concat [acc, concat as2, concat as1]

_iter_alg2 :: [Algo] -> Int -> [Algo]
_iter_alg2 algs n = filter (\(Algo name _ _ _) -> name `notElem` [_BASE_name, _REP_name])
                      $ _iter_alg algs n

_print_algs :: Int -> IO()
_print_algs n = mapM_ print $ sort $ _iter_alg2 _base_algs n
----------------------------------------------------------------------------------------------

main :: IO()
main = do
    args <- getArgs
    _print_algs (read $ head args)


