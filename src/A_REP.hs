module A_REP (_REP_Alg) where

import Algo
import AlgoUtils
import Constants
import PortCapability

{-# ANN module "HLint: ignore Use camelCase" #-}

-- REP pc1 pc2 where pc1 >= nRmW and pc2 >= n'RmW => (n+n')RmW
_REP1 :: PortCapability -> PortCapability -> Int -> Int -> Int -> Maybe PortCapability
_REP1 pc1 pc2 n n' m
    | (pc1 `covers` bank1) && (pc2 `covers` bank2) = Just algo
    | otherwise = Nothing
    where bank1 = pc_nRmW n m
          bank2 = pc_nRmW n' m
          algo = pc_nRmW (n+n') m

_REP :: PortCapability -> PortCapability -> [PortCapability]
_REP pc1 pc2 = filter_redundant_pcs $ pc_fn3 $ _REP1 pc1 pc2

_REP_Alg :: Algo -> Algo -> [Algo]
_REP_Alg = alg_fn2
               max -- pick the max level of the two algos
               (const True) -- no restriction on level
               _REP_name
               _REP
               (\name pc lvl _ -> Algo name pc lvl []) -- no dependencies
