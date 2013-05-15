module AlgoUtils where

import Algo
import Constants
import Control.Monad (MonadPlus, mzero)
import Data.List (intercalate)
import Data.Maybe (isJust, fromJust)
import PortCapability

{-# ANN module "HLint: ignore Use camelCase" #-}

ceil2 :: Int -> Int
ceil2 n = n `div` 2 + n `rem` 2

when :: (MonadPlus m) => Bool -> m a -> m a
when p v = if p then v else mzero

pc_fn1 :: (Int -> Maybe PortCapability) -> [PortCapability]
pc_fn1 fn1 = let pcs = takeWhile isJust $ map fn1 [1..]
             in if null pcs then [] else [fromJust $ last pcs]

pc_fn2 :: (Int -> Int -> Maybe PortCapability) -> [PortCapability]
pc_fn2 fn2 = let pcs = takeWhile (not.null) $ map (pc_fn1 . fn2) [1..]
             in filter_redundant_pcs $ concat pcs

pc_fn3 :: (Int -> Int -> Int -> Maybe PortCapability) -> [PortCapability]
pc_fn3 fn3 = let pcs = takeWhile (not.null) $ map (pc_fn2 . fn3) [1..]
             in filter_redundant_pcs $ concat pcs

pc_noop :: Maybe PortCapability
pc_noop = Just $ portCap "NOOP"

pc_nR :: Int -> Maybe PortCapability
pc_nR 0 = pc_noop
pc_nR n
    | n <= _MAX_N_NR = Just $ portCap $ show n ++ "R"
    | otherwise = Nothing

pc_nW :: Int -> Maybe PortCapability
pc_nW 0 = pc_noop
pc_nW n
    | n <= _MAX_N_NW = Just $ portCap $ show n ++ "W"
    | otherwise = Nothing

pc_nRW :: Int -> Maybe PortCapability
pc_nRW 0 = pc_noop
pc_nRW n
    | n <= _MAX_N_NRW = Just $ portCap $ show n ++ "RW"
    | otherwise = Nothing

pc_nRor1W :: Int -> Maybe PortCapability
pc_nRor1W 0 = pc_nW 1
pc_nRor1W n
    | n <= _MAX_N_NRor1W = Just $ portCap $ show n ++ "Ror1W"
    | otherwise = Nothing

pc_nRmW :: Int -> Int -> Maybe PortCapability
pc_nRmW 0 0 = pc_noop
pc_nRmW 0 m = pc_nW m
pc_nRmW n 0 = pc_nR n
pc_nRmW n m
    | (n+m) <= _MAX_N_PLUS_M_NRMW = Just $ portCap $ show n ++ "R" ++ show m ++ "W"
    | otherwise = Nothing

pc_ORed :: [PortCapability] -> Maybe PortCapability
pc_ORed = Just . portCap . intercalate "or" . map show

pc_ORed' :: [Maybe PortCapability] -> Maybe PortCapability
pc_ORed' pcs = sequence pcs >>= pc_ORed

pc_dotProd2Ror1W :: Int -> Maybe PortCapability
pc_dotProd2Ror1W n = _pcor_to_portCap $ foldr1 _dot_prod $ replicate n (2,1,[])

-- (n, m, [pq])
-- (nR OR mW OR [pRqW])
type PCOR = (Int, Int, [(Int, Int)])

_pcor_to_portCap :: PCOR -> Maybe PortCapability
_pcor_to_portCap (n, m, pqs)
    = (>>=) (sequence ([pc_nR n, pc_nW m] ++
                       [pc_nRmW p q | (p,q) <- pqs]))
            (pc_ORed . filter_redundant_pcs)

_dot_prod :: PCOR -> PCOR -> PCOR
_dot_prod (r, w, rws) (r', w', r'w's)
    = (r+r', w+w', [(r,w'), (r',w)]
                   ++ [(r+a,b)   | (a,b) <- r'w's]
                   ++ [(c,w+d)   | (c,d) <- r'w's]
                   ++ [(e+r',f)  | (e,f) <- rws]
                   ++ [(g,h+w')  | (g,h) <- rws])

filter_redundant :: (a -> a -> Bool) -> [a] -> [a]
filter_redundant covers_fn = fr []
    where fr goods [] = goods
          fr goods (a:rems)
              | any (`covers_fn` a) $ goods ++ rems = fr goods rems
              | otherwise = fr (a:goods) rems

filter_redundant_pcs :: [PortCapability] -> [PortCapability]
filter_redundant_pcs = filter_redundant covers

alg_fn2 ::   (Int -> Int -> Int) -- level incing fn
          -> (Int -> Bool) -- lvl predicate
          ->  String -- Algo name
          -> (PortCapability -> PortCapability -> [PortCapability]) -- pc fn
          -> (String -> PortCapability -> Int -> [PortCapability] -> Algo) -- algo cons
          ->  Algo -> Algo -> [Algo]
alg_fn2 lvlfn lvlpred alg_name alg_pc_fn alg_cons (Algo _ pc1 lvl1 _ ) (Algo _ pc2 lvl2 _)
    | lvlpred lvl = alg_pc_fn pc1 pc2 >>= (\pc -> [alg_cons alg_name pc lvl [pc1, pc2]])
    | otherwise   = []
    where lvl = lvlfn lvl1 lvl2

alg_fn1 ::   (Int -> Int) -- level incing fn
          -> (Int -> Bool) -- lvl predicate
          ->  String -- Algo name
          -> (PortCapability -> [PortCapability]) -- pc fn
          -> (String -> PortCapability -> Int -> [PortCapability] -> Algo) -- algo cons
          ->  Algo -> [Algo]
alg_fn1 lvlfn lvlpred alg_name alg_pc_fn alg_cons (Algo _ pc1 lvl1 _)
    | lvlpred lvl  = alg_pc_fn pc1 >>= (\pc -> [alg_cons alg_name pc lvl [pc1]])
    | otherwise    = []
    where lvl = lvlfn lvl1

