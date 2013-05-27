module Algos where

import A_MT
import A_REP
import A_RL
import A_XR
import Algo
import AlgoUtils
import Constants
--import Control.DeepSeq (deepseq)
--import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (using, rdeepseq, rpar, evalList, dot)
import qualified Data.HashMap.Lazy as HM (empty, lookupDefault, insert, elems)
import Data.List (sort)
import Data.Maybe (fromJust)
--import Debug.Trace (trace)
import PortCapability
import System.Environment
--import System.IO.Unsafe (unsafePerformIO)

{-# ANN module "HLint: ignore Use camelCase" #-}

_base_pcs :: [PortCapability]
_base_pcs = map fromJust [pc_nRW 1, pc_nRmW 1 1, pc_nRW 2, pc_nRmW 2 2]

_base_algs :: [Algo]
_base_algs = map (\pc -> Algo _BASE_name pc 0 []) _base_pcs

_ALG_fns2 :: [Algo -> Algo -> [Algo]]
_ALG_fns2 = concat [_MT_Alg, _RL_Alg, _REP_Alg]

_ALG_fns1 :: [Algo -> [Algo]]
_ALG_fns1 = _XR_Alg

-- assuming both algos have same name and portcap
_alg_covers :: Algo -> Algo -> Bool
(Algo _ _ lvl1 pcs1) `_alg_covers` (Algo _ _ lvl2 pcs2)
    | lvl1 <= lvl2
        = and $ zipWith covers pcs2 pcs1
    | otherwise = False

-- only algos from one group are passed in to this function
_filter_redundant_algs :: [Algo] -> [Algo]
_filter_redundant_algs = filter_redundant _alg_covers

_group_algs :: [Algo] -> [[Algo]]
_group_algs = HM.elems . foldr
                  (\alg@(Algo name pc _ _) acc ->
                      HM.insert (name, pc) (alg:HM.lookupDefault [] (name, pc) acc) acc)
                  HM.empty

_keep_good_algs :: [[Algo]] -> [Algo]
_keep_good_algs algss = let grouped_algs = _group_algs $ concat algss
                            filtered_algs = map _filter_redundant_algs grouped_algs
                                                `using` evalList (rpar `dot` rdeepseq)
                        in concat filtered_algs

_eval_alg2 :: (Algo -> Algo -> [Algo]) -> [Algo] -> [Algo]
_eval_alg2 alg_fn algs = _keep_good_algs [alg_fn a1 a2 | a1 <- algs, a2 <- algs]

_eval_alg1 :: (Algo -> [Algo]) -> [Algo] -> [Algo]
_eval_alg1 alg_fn algs = _keep_good_algs [alg_fn a | a <- algs]

_iter_alg :: [Algo] -> Int -> [Algo]
_iter_alg algs n = foldr _iter algs [1..n]
    where _iter _ acc = let fns = map _eval_alg2 _ALG_fns2 ++
                                  map _eval_alg1 _ALG_fns1
                            as' = map (\fn -> fn acc) fns
                                    `using` evalList (rpar `dot` rdeepseq)
                            bs = _keep_good_algs [acc, concat as']
                        in bs

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


