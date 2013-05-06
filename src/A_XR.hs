module A_XR (_XR_Alg, _XR_nRW) where

import Algo
import AlgoUtils
import Constants
import Data.List (intercalate)
import Data.Maybe (fromJust)
import PortCapability

{-# ANN module "HLint: ignore Use camelCase" #-}

-- 1.
-- XR pc where pc >= nRor1W => 2nRor1W

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

-- 2.
-- XR pc where pc >= nRW => (2Ror1W)^n

-- 3.
-- XR pc where pc >= nRmW => OR (n'Rm'W, n'+m' = n+m, 0<=m'<=m, n'<=2*n)

_XR1 :: PortCapability -> Int -> Maybe PortCapability
_XR1 pc n
    | pc `covers` bank = Just algo
    | otherwise = Nothing
    where bank = pc_nRor1W n
          algo = pc_nRor1W (2^n)

_XR2 :: PortCapability -> Int -> Maybe PortCapability
_XR2 pc n
    | pc `covers` bank = Just algo
    | otherwise = Nothing
    where bank = pc_nRW n
          algo = pcorToPortCap $ foldr1 _dot_prod $ replicate n (2,1,[])

_XR3 :: PortCapability -> Int -> Int -> Maybe PortCapability
_XR3 pc n m
    | pc `covers` bank = Just algo
    | otherwise = Nothing
    where bank = pc_nRmW n m
          algo = portCap $ intercalate "or" $ map show
                           [pc_nRmW n' m' | m' <- [0..m],
                                let n' = n+m-m', n' <= 2*n]

-- convinience fn
_XR_nRW :: Int -> PortCapability
_XR_nRW n = fromJust $ _XR2 (pc_nRW n) n

_XR :: PortCapability -> [PortCapability]
_XR pc = filter_redundant_pcs $ concat [pc_fn1 $ _XR1 pc,
                                        pc_fn1 $ _XR2 pc,
                                        pc_fn2 $ _XR3 pc]

-- (n, m, [pq])
-- (nR OR mW OR [pRqW])
type PCOR = (Int, Int, [(Int, Int)])

pcorToPortCap :: PCOR -> PortCapability
pcorToPortCap (n, m, pqs)
    = portCap $ intercalate "or" $ map show
              $ filter_redundant_pcs ([pc_nR n, pc_nW m] ++
                                      [pc_nRmW p q | (p,q) <- pqs])

_dot_prod :: PCOR -> PCOR -> PCOR
_dot_prod (r, w, rws) (r', w', r'w's)
    = (r+r', w+w', [(r,w'), (r',w)]
                   ++ [(r+a,b)   | (a,b) <- r'w's]
                   ++ [(c,w+d)   | (c,d) <- r'w's]
                   ++ [(e+r',f)  | (e,f) <- rws]
                   ++ [(g,h+w')  | (g,h) <- rws])

_XR_Alg :: Algo -> [Algo]
_XR_Alg = alg_fn1
              (+1) -- inc level
              (<= _MAX_XR_LEVEL) -- max level
              _XR_name
              _XR
              Algo
