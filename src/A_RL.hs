{-# LANGUAGE DeriveDataTypeable #-}
module A_RL (_RL_Alg) where

import A_XR (_XR_nRW)
import AlgoRegistry
import AlgoUtils
import Constants
import Data.Typeable
import PortCapability

{-# ANN module "HLint: ignore Use camelCase" #-}

type Bank = PortCapability
type Tag = PortCapability
type Lvl = Int

-- 1.
-- RL pc1 pc2 where pc1 >= XR(nRW) and pc2 >= 2nRnW => 2nRW

-- 2.
-- RL pc1 pc2 where pc1 >= nRor1W and pc2 >= (n+m)RmW => nRmW

-- 3.
-- RL pc1 pc2 where pc1 >= nRmW and pc2 >= (n+m+x)RmW => nR(m+x)W

_RL1 :: Int -> Maybe (Bank, Tag, PortCapability)
_RL1 n = do
    bank  <- _XR_nRW n
    tag   <- pc_nRmW (2*n) n
    algo  <- pc_nRW (2*n)
    return (bank, tag, algo)

_RL2 :: Int -> Int -> Maybe (Bank, Tag, PortCapability)
_RL2 n m = do
    bank  <- pc_nRor1W n
    tag   <- pc_nRmW (n+m) m
    algo  <- pc_nRmW n m
    return (bank, tag, algo)

_RL3 :: Int -> Int -> Int -> Maybe (Bank, Tag, PortCapability)
_RL3 n m x = do
    bank  <- pc_nRmW n m
    tag   <- pc_nRmW (n+m+x) m
    algo  <- pc_nRmW n (m+x)
    return (bank, tag, algo)

data A_RL = A_RL PortCapability Lvl Bank Tag deriving (Eq, Typeable)
instance AlgoLike A_RL where
    getName _ = _RL_name
    getPortCap (A_RL pc _ _ _) = pc
    getDeps (A_RL _ _ bank tag) = [bank, tag]
    getLvl (A_RL _ lvl _ _) = lvl

_to_A_RL :: AlgoRegistry -> (Bank, Tag, PortCapability) -> [Algo]
_to_A_RL ar (bank, tag, algo) = do
    (bankLvl, bank') <- getFromReg bank ar
    (tagLvl, tag') <- getFromReg tag ar
    let algoLvl = max bankLvl tagLvl + 1
    when (algoLvl <= _MAX_RL_LEVEL) $
        return $ toAlgo $ A_RL algo algoLvl bank' tag'

_RL_Alg :: [AlgoRegistry -> [Algo]]
_RL_Alg
    = map (\f -> f _to_A_RL)
        [alg_fn1 _RL1,
         alg_fn2 _RL2,
         alg_fn3 _RL3]
