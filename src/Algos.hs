module Algos
(
) where

import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.List (sort)
import qualified Data.HashSet as HS (HashSet, fromList, toList, unions, map, filter)

{-# ANN module "HLint: ignore Use camelCase" #-}

data PC = PC_nRmW Int Int
        | PC_nRW Int
        | PC_nRor1W Int
        deriving (Eq, Ord)

instance Hashable PC where
    hashWithSalt salt (PC_nRmW n m) = salt * n * m
    hashWithSalt salt (PC_nRW n) = salt * 2 * n
    hashWithSalt salt (PC_nRor1W n) = salt * 3 * n

instance Show PC where
    show (PC_nRmW n m) = show n ++ "R" ++ show m ++ "W"
    show (PC_nRW n) = show n ++ "RW"
    show (PC_nRor1W n) = show n ++ "Ror1W"

_MT  :: PC -> PC -> Maybe PC
_RL  :: PC -> PC -> Maybe PC
_XR  :: PC -> Maybe PC
_REP :: PC -> PC -> Maybe PC

_MT (PC_nRW n) (PC_nRmW n' m)
    | (n+m)==n' = Just $ PC_nRmW n m

_MT (PC_nRor1W n) (PC_nRmW n' m)
    | (n+m)==n' && n==m = Just $ PC_nRW n
    
_MT _ _ = Nothing

_RL (PC_nRW n) (PC_nRmW n' m)
    | (n+m)==n' && n==m = Just $ PC_nRmW n m
    
_RL (PC_nRor1W n) (PC_nRmW n' 1)
    | (n+1)==n' = Just $ PC_nRW n

_RL _ _ = Nothing

_XR (PC_nRW 1) = Just $ PC_nRor1W 2
_XR (PC_nRor1W n) = Just $ PC_nRor1W (2*n)
_XR _ = Nothing

_REP (PC_nRmW n m) (PC_nRmW n' m')
    | m == m' = Just $ PC_nRmW (n+n') m

_REP _ _ = Nothing

----------------------------------------------------------------------------------------------
filterNothings :: [Maybe a] -> [a]
filterNothings = foldr (\ma acc -> case ma of
                                       Nothing -> acc
                                       Just a -> a:acc)
                 []

_iter :: (Eq k, Hashable k) => [k -> k -> Maybe k] -> [k -> Maybe k] -> HS.HashSet k -> HS.HashSet k
_iter fns2 fns1 ks = let ks1 = [fn k1 k2 | k1 <- HS.toList ks, k2 <- HS.toList ks, fn <- fns2]
                         ks2 = [fn k | k <- HS.toList ks, fn <- fns1]
                     in HS.unions (ks: map (HS.fromList.filterNothings) [ks1, ks2])

basePcs :: HS.HashSet PC
basePcs = HS.fromList [PC_nRW 1, PC_nRW 2, PC_nRmW 1 1, PC_nRmW 2 2]

_iter_pc :: HS.HashSet PC -> Int -> HS.HashSet PC
_iter_pc pcs n = foldr (\_ acc -> _iter [_MT, _RL, _REP] [_XR] acc) pcs [1..n]
----------------------------------------------------------------------------------------------

_MT_name :: String; _MT_name = "_MT"
_RL_name :: String; _RL_name = "_RL"
_XR_name :: String; _XR_name = "_XR"
_REP_name :: String; _REP_name = "_REP"
_BASE_name :: String; _BASE_name = "_BASE"

_MAX_MT_LEVEL :: Int; _MAX_MT_LEVEL = 4
_MAX_RL_LEVEL :: Int; _MAX_RL_LEVEL = 4
_MAX_XR_LEVEL :: Int; _MAX_XR_LEVEL = 3

data Algo = Algo String PC Int [Algo] deriving (Eq, Ord)

instance Hashable Algo where
    hashWithSalt salt (Algo name pc lvl algs)
        = foldr hashWithSalt salt $ [hash name, hash pc, lvl] ++ map hash algs

instance Show Algo where
    show (Algo name pc _ algs)
        = show pc ++ (if name==_BASE_name then "" else name)
          ++ (if null algs then "" else "::" ++ show algs)

_Alg2 :: (Int -> Int -> Int) -> (Int -> Bool) -> String -> (PC -> PC -> Maybe PC)
          -> (String -> PC -> Int -> [Algo] -> Algo) -> Algo -> Algo -> Maybe Algo
_Alg2 lvlfn lvlpred alg_name alg_pc_fn alg_cons alg1@(Algo _ pc1 lvl1 _ ) alg2@(Algo _ pc2 lvl2 _)
    | lvlpred lvl  = alg_pc_fn pc1 pc2 >>= (\pc -> Just $ alg_cons alg_name pc lvl [alg1, alg2])
    | otherwise    = Nothing
    where lvl = lvlfn lvl1 lvl2

_Alg1 :: (Int -> Int) -> (Int -> Bool) -> String -> (PC -> Maybe PC)
         -> (String -> PC -> Int -> [Algo] -> Algo) -> Algo -> Maybe Algo
_Alg1 lvlfn lvlpred alg_name alg_pc_fn alg_cons alg1@(Algo _ pc1 lvl1 _)
    | lvlpred lvl  = alg_pc_fn pc1 >>= (\pc -> Just $ alg_cons alg_name pc lvl [alg1])
    | otherwise    = Nothing
    where lvl = lvlfn lvl1

_MT_Alg :: Algo -> Algo -> Maybe Algo
_MT_Alg = _Alg2 (\lvl1 lvl2 -> max lvl1 lvl2 + 1) (<=_MAX_MT_LEVEL) _MT_name _MT Algo

_RL_Alg :: Algo -> Algo -> Maybe Algo
_RL_Alg = _Alg2 (\lvl1 lvl2 -> max lvl1 lvl2 + 1) (<=_MAX_RL_LEVEL) _RL_name _RL Algo

_XR_Alg :: Algo -> Maybe Algo
_XR_Alg = _Alg1 (+1) (<=_MAX_XR_LEVEL) _XR_name _XR Algo

_REP_Alg :: Algo -> Algo -> Maybe Algo
_REP_Alg = _Alg2 max (const True) _REP_name _REP (\name pc _ _ -> Algo name pc 0 [])

baseAlgs :: HS.HashSet Algo
baseAlgs = HS.map base basePcs
    where base pc = Algo _BASE_name pc 0 []

_iter_alg :: HS.HashSet Algo -> Int -> HS.HashSet Algo
_iter_alg algs n = foldr (\_ acc -> _iter [_MT_Alg, _RL_Alg, _REP_Alg] [_XR_Alg] acc) algs [1..n]

_iter_alg2 :: HS.HashSet Algo -> Int -> HS.HashSet Algo
_iter_alg2 algs n = HS.filter (\(Algo name _ _ _) -> name `notElem` [_BASE_name, _REP_name])
                      $ _iter_alg algs n

_print_algs :: Int -> IO()
_print_algs n = mapM_ print $ sort $ HS.toList $ _iter_alg2 baseAlgs n
----------------------------------------------------------------------------------------------


