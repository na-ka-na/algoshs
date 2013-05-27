module A_XR (_XR_Alg, _XR_nRW) where

import Algo
import AlgoUtils
import Constants
import PortCapability

{-# ANN module "HLint: ignore Use camelCase" #-}

-- 1.
-- XR pc where pc >= nRor1W => 2nRor1W

-- 2.
-- XR pc where pc >= nRW => (2Ror1W)^n

-- 3.
-- XR pc where pc >= nRmW => OR (n'Rm'W, n'+m' = n+m, 0<=m'<=m, n'<=2*n)

-- Uncovered
-- 3R1W_dp_XR (missing corner)
-- 3Ror1W_XR (missing corner)
-- 2R1W_2p_XR (missing corner)

_XR1 :: PortCapability -> Int -> Maybe PortCapability
_XR1 pc n = do
    bank <- pc_nRor1W n
    algo <- pc_nRor1W (2^n)
    when (pc `covers` bank) $ return algo

_XR2 :: PortCapability -> Int -> Maybe PortCapability
_XR2 pc n = do
    bank <- pc_nRW n
    algo <- pc_dotProd2Ror1W n
    when (pc `covers` bank) $ return algo

_XR3 :: PortCapability -> Int -> Int -> Maybe PortCapability
_XR3 pc n m = do
    bank <- pc_nRmW n m
    algo <- pc_ORed' [pc_nRmW n' m' | m' <- [0..m],
                         let n' = n+m-m', n' <= 2*n]
    when (pc `covers` bank) $ return algo

-- convinience fn
_XR_nRW :: Int -> Maybe PortCapability
_XR_nRW n = pc_nRW n >>= (`_XR2` n)

_XR_Alg :: [Algo -> [Algo]]
_XR_Alg = map (alg_fn1
                 (+1) -- inc level
                 (<= _MAX_XR_LEVEL) -- max level
                 _XR_name
                 Algo)
              [pc_fn1 . _XR1,
               pc_fn1 . _XR2,
               pc_fn2 . _XR3]
