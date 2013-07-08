{-# LANGUAGE DeriveDataTypeable #-}
module A_XR (_XR_nRW, _XR_Alg) where

import AlgoRegistry
import AlgoUtils
import Constants
import Data.Typeable
import PortCapability

{-# ANN module "HLint: ignore Use camelCase" #-}

type Bank = PortCapability
type Lvl = Int

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

_XR1 :: Int -> Maybe (Bank, PortCapability)
_XR1 n = do
    bank <- pc_nRor1W n
    algo <- pc_nRor1W (2^n)
    return (bank, algo)

_XR2 :: Int -> Maybe (Bank, PortCapability)
_XR2 n = do
    bank <- pc_nRW n
    algo <- pc_dotProd2Ror1W n
    return (bank, algo)

_XR3 :: Int -> Int -> Maybe (Bank, PortCapability)
_XR3 n m = do
    bank <- pc_nRmW n m
    algo <- pc_ORed' [pc_nRmW n' m' | m' <- [0..m],
                         let n' = n+m-m', n' <= 2*n]
    return (bank, algo)

-- convinience fn
_XR_nRW :: Int -> Maybe PortCapability
_XR_nRW n = _XR2 n >>= (\(_, algo) -> Just algo)

data A_XR = A_XR PortCapability Lvl Bank deriving (Eq, Typeable)
instance AlgoLike A_XR where
    getName _ = _XR_name
    getPortCap (A_XR pc _ _) = pc
    getDeps (A_XR _ _ bank) = [bank]
    getLvl (A_XR _ lvl _) = lvl

_to_A_XR :: AlgoRegistry -> (Bank, PortCapability) -> [Algo]
_to_A_XR ar (bank, algo) = do
    (bankLvl, bank') <- getFromReg bank ar
    let algoLvl = bankLvl + 1
    when (algoLvl <= _MAX_XR_LEVEL) $
        return $ toAlgo $ A_XR algo algoLvl bank'

_XR_Alg :: [AlgoRegistry -> [Algo]]
_XR_Alg
    = map (\f -> f _to_A_XR)
        [alg_fn1 _XR1,
         alg_fn1 _XR2,
         alg_fn2 _XR3]
