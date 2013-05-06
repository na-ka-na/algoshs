module Algos where

import A_MT
import A_REP
import A_RL
import A_XR
import Algo
import AlgoUtils
import Constants
import qualified Data.HashMap.Lazy as HM (empty, lookupDefault, insert, elems)
import Data.List (sort)
import PortCapability
import System.Environment

{-# ANN module "HLint: ignore Use camelCase" #-}
----------------------------------------------------------------------------------------------------
-- 1.
-- MT pc1 pc2 where pc1 >= XR(nRW) and pc2 >= 2nRnW => 2nRW
-- 2RW_MT2, 4RW_dp_MT

-- 2.
-- MT pc1 pc2 where pc1 >= nRor1W and pc2 >= (n+m)RmW => nRmW
-- 1R2W_1p_MT, 1R3W_1p_MT, 1R4W_1p_MT, 2R1W_1p_MT, 2R1W_dp_MT, 2R2W_1p_MT, 2R3W_dp_MT, 2R4W_dp_MT, 3R1W_1p_MT

-- 3.
-- Is this subset of 2. ?
-- MT pc1 pc2 where pc1 >= nRW and pc2 >= 2nR(n/2)W => nRnW
-- 1R1W_MT, 2R2W_dp_MT
-- if we change => to nRmW or 2mW, then we can cover
-- 1R1Wor2W_MT

-- Uncovered
-- 2R2W_2p_MT (2Ror1R1W, 4R2W)
-- 1R8W_1R1W_MT (n*1R1W, 8*1R1W) wtf
-- 4R4W_4p_MT (4*n*1R1W, 3*4*1R1W)
--

-- 1.
-- XR pc where pc >= nRor1W => 2nRor1W

-- 2.
-- XR 1R1W => 2Ror1R1W
-- XR 2RW => 4Ror2R1Wor2W
-- XR 2Ror1R1W => 3R1W

-- 2Ror1R1Wor2W

-- Uncovered
-- 3R1W_dp_XR (missing corner)
-- 3Ror1W_XR (missing corner)
-- 2R1W_2p_XR (missing corner)
-- 4R1W_2p_XR (XR.XR 2RW)
-- 4Ror2R1W_XR (XR 2R1W)
-- 6Ror4R1W_XR (XR 3R1W)

-- 3.
-- XR pc where pc >= nRW => (2Ror1W)^n

-- 4.
-- XR pc where pc >= nRmW, n>=m => OR (n'Rm'W, n'+m' = n+m, 0<=m'<=m)


{- this is a comment
   RL
   
   1.
   XR(1RW),   2R1W      =>  2RW
   XR(2RW),   4R2W      =>  4RW
   
   2.
   1RW,       2R1W      =>  1R1W
   1RW,       3R2W      =>  1R2W
   2Ror1W,    3R1W      =>  2R1W
   2Ror1W,    4R2W      =>  2R2W
   3Ror1W,    4R1W      =>  3R1W
   
   
   3.
   1R1W,      3R1W      =>  1R2W
   1R1W,      4R1W      =>  1R3W
   2R1W,      4R1W      =>  2R2W
   4R2W,      8R2W      =>  4R4W
   2R2W,      5R2W      =>  2R3W
   2R2W,      6R2W      =>  2R4W
   
-}

-- 1.
-- RL pc1 pc2 where pc1 >= XR(nRW) and pc2 >= 2nRnW => 2nRW

-- 2.
-- RL pc1 pc2 where pc1 >= nRor1W and pc2 >= (n+m)RmW => nRmW

-- 3.
-- RL pc1 pc2 where pc1 >= nRmW and pc2 >= (n+m+x)RmW => nR(m+x)W

----------------------------------------------------------------------------------------------------

_base_pcs :: [PortCapability]
_base_pcs = [pc_nRW 1, pc_nRmW 1 1, pc_nRW 2, pc_nRmW 2 2]

_base_algs :: [Algo]
_base_algs = map (\pc -> Algo _BASE_name pc 0 []) _base_pcs

_ALG_fns2 :: [Algo -> Algo -> [Algo]]
_ALG_fns2 = [_MT_Alg, _RL_Alg, _REP_Alg]

_ALG_fns1 :: [Algo -> [Algo]]
_ALG_fns1 = [_XR_Alg]

-- assuming both algos have same name and portcap
_alg_covers :: Algo -> Algo -> Bool
(Algo _ _ lvl1 pcs1) `_alg_covers` (Algo _ _ lvl2 pcs2)
    | lvl1 <= lvl2
        = and $ zipWith covers pcs2 pcs1
    | otherwise = False

-- only algos from one group are passed in to this function
_filter_redundant_algs :: [Algo] -> [Algo]
_filter_redundant_algs = fra []
    where fra good_algs [] = good_algs
          fra good_algs (alg:rem_algs)
              | any (`_alg_covers` alg) $ good_algs ++ rem_algs = fra good_algs rem_algs
              | otherwise = fra (alg:good_algs) rem_algs

_group_algs :: [Algo] -> [[Algo]]
_group_algs = HM.elems . foldr
                  (\alg@(Algo name pc _ _) acc ->
                      HM.insert (name, pc) (alg:HM.lookupDefault [] (name, pc) acc) acc)
                  HM.empty

_iter_alg :: [Algo] -> Int -> [Algo]
_iter_alg algs n = foldr _iter algs [1..n]
    where _iter _ acc = let as2 = [fn a1 a2 | a1 <- acc, a2 <- acc, fn <- _ALG_fns2]
                            as1 = [fn a | a <- acc, fn <- _ALG_fns1]
                        in concatMap _filter_redundant_algs
                            $ _group_algs
                            $ concat [acc, concat as2, concat as1]

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


