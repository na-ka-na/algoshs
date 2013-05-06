module A_RL (_RL_Alg) where

import Algo
import A_XR (_XR_nRW)
import AlgoUtils
import Constants
import PortCapability

{-# ANN module "HLint: ignore Use camelCase" #-}

-- 1.
-- RL pc1 pc2 where pc1 >= XR(nRW) and pc2 >= 2nRnW => 2nRW

-- 2.
-- RL pc1 pc2 where pc1 >= nRor1W and pc2 >= (n+m)RmW => nRmW

-- 3.
-- RL pc1 pc2 where pc1 >= nRmW and pc2 >= (n+m+x)RmW => nR(m+x)W

_RL1 :: PortCapability -> PortCapability -> Int -> Maybe PortCapability
_RL1 pc1 pc2 n
    | (pc1 `covers` bank) && (pc2 `covers` tag) = Just algo
    | otherwise = Nothing
    where bank  = _XR_nRW n
          tag   = pc_nRmW (2*n) n
          algo  = pc_nRW (2*n)

_RL2 :: PortCapability -> PortCapability -> Int -> Int -> Maybe PortCapability
_RL2 pc1 pc2 n m
    | (pc1 `covers` bank) && (pc2 `covers` tag) = Just algo
    | otherwise = Nothing
    where bank  = pc_nRor1W n
          tag   = pc_nRmW (n+m) m
          algo  = pc_nRmW n m

_RL3 :: PortCapability -> PortCapability -> Int -> Int -> Int -> Maybe PortCapability
_RL3 pc1 pc2 n m x
    | (pc1 `covers` bank) && (pc2 `covers` tag) = Just algo
    | otherwise = Nothing
    where bank  = pc_nRmW n m
          tag   = pc_nRmW (n+m+x) m
          algo  = pc_nRmW n (m+x)

_RL :: PortCapability -> PortCapability -> [PortCapability]
_RL pc1 pc2 = filter_redundant_pcs $ concat [pc_fn1 $ _RL1 pc1 pc2,
                                             pc_fn2 $ _RL2 pc1 pc2,
                                             pc_fn3 $ _RL3 pc1 pc2]

_RL_Alg :: Algo -> Algo -> [Algo]
_RL_Alg = alg_fn2
              (\lvl1 lvl2 -> max lvl1 lvl2 + 1) -- inc level
              (<= _MAX_RL_LEVEL) -- max level
              _RL_name
              _RL
              Algo
