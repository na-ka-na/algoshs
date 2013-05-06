module A_MT (_MT_Alg) where

import Algo
import A_XR (_XR_nRW)
import Constants
import AlgoUtils
import PortCapability

{-# ANN module "HLint: ignore Use camelCase" #-}

-- 1.
-- MT pc1 pc2 where pc1 >= XR(nRW) and pc2 >= 2nRnW => 2nRW

-- 2.
-- MT pc1 pc2 where pc1 >= nRor1W and pc2 >= (n+m)RmW => nRmW

-- 3.
-- MT pc1 pc2 where pc1 >= nRW and pc2 >= 2nR(n/2)W => nRnW
-- if we change => to nRmW or 2mW, then we can cover 1R1Wor2W

_MT1 :: PortCapability -> PortCapability -> Int -> Maybe PortCapability
_MT1 pc1 pc2 n
    | (pc1 `covers` bank) && (pc2 `covers` state) = Just algo
    | otherwise = Nothing
    where bank  = _XR_nRW n
          state = pc_nRmW (2*n) n
          algo  = pc_nRW (2*n)

_MT2 :: PortCapability -> PortCapability -> Int -> Int -> Maybe PortCapability
_MT2 pc1 pc2 n m
    | (pc1 `covers` bank) && (pc2 `covers` state) = Just algo
    | otherwise = Nothing
    where bank  = pc_nRor1W n
          state = pc_nRmW (n+m) m
          algo  = pc_nRmW n m

_MT3 :: PortCapability -> PortCapability -> Int -> Maybe PortCapability
_MT3 pc1 pc2 n
    | (pc1 `covers` bank) && (pc2 `covers` state) = Just algo
    | otherwise = Nothing
    where bank  = pc_nRW n
          state = pc_nRmW (2*n) (ceil2 n)
          algo  = pc_nRmW n n

_MT :: PortCapability -> PortCapability -> [PortCapability]
_MT pc1 pc2 = filter_redundant_pcs $ concat [pc_fn1 $ _MT1 pc1 pc2,
                                             pc_fn2 $ _MT2 pc1 pc2,
                                             pc_fn1 $ _MT3 pc1 pc2]
_MT_Alg :: Algo -> Algo -> [Algo]
_MT_Alg = alg_fn2
              (\lvl1 lvl2 -> max lvl1 lvl2 + 1) -- inc level
              (<= _MAX_MT_LEVEL) -- max level
              _MT_name
              _MT
              Algo
