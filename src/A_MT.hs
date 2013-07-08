{-# LANGUAGE DeriveDataTypeable #-}
module A_MT (_MT_Alg) where

import A_XR (_XR_nRW)
import AlgoRegistry
import AlgoUtils
import Constants
import Data.Typeable
import PortCapability

{-# ANN module "HLint: ignore Use camelCase" #-}

type Bank = PortCapability
type State = PortCapability
type Lvl = Int

-- 1.
-- MT pc1 pc2 where pc1 >= XR(nRW) and pc2 >= 2nRnW => 2nRW

-- 2.
-- MT pc1 pc2 where pc1 >= nRor1W and pc2 >= (n+m)RmW => nRmW

-- 3.
-- MT pc1 pc2 where pc1 >= nRW and pc2 >= 2nR(n/2)W => nRnW
-- if we change => to nRmW or 2mW, then we can cover 1R1Wor2W

_MT1 :: Int -> Maybe (Bank, State, PortCapability)
_MT1 n = do
    bank  <- _XR_nRW n
    state <- pc_nRmW (2*n) n
    algo  <- pc_nRW (2*n)
    return (bank, state, algo)

_MT2 :: Int -> Int -> Maybe (Bank, State, PortCapability)
_MT2 n m = do
    bank  <- pc_nRor1W n
    state <- pc_nRmW (n+m) m
    algo  <- pc_nRmW n m
    return (bank, state, algo)

_MT3 :: Int -> Maybe (Bank, State, PortCapability)
_MT3 n = do
    bank  <- pc_nRW n
    state <- pc_nRmW (2*n) (ceil2 n)
    algo  <- pc_nRmW n n
    return (bank, state, algo)

data A_MT = A_MT PortCapability Lvl Bank State deriving (Eq, Typeable)
instance AlgoLike A_MT where
    getName _ = _MT_name
    getPortCap (A_MT pc _ _ _) = pc
    getDeps (A_MT _ _ bank state) = [bank, state]
    getLvl (A_MT _ lvl _ _) = lvl

_to_A_MT :: AlgoRegistry -> (Bank, State, PortCapability) -> [Algo]
_to_A_MT ar (bank, state, algo) = do
    (bankLvl, bank') <- getFromReg bank ar
    (stateLvl, state') <- getFromReg state ar
    let algoLvl = max bankLvl stateLvl + 1
    when (algoLvl <= _MAX_MT_LEVEL) $
        return $ toAlgo $ A_MT algo algoLvl bank' state'

_MT_Alg :: [AlgoRegistry -> [Algo]]
_MT_Alg
    = map (\f -> f _to_A_MT)
        [alg_fn1 _MT1,
         alg_fn2 _MT2,
         alg_fn1 _MT3]
