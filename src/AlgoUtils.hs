{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
module AlgoUtils where

import Constants
import Control.DeepSeq (NFData(..), deepseq)
import Control.Monad (MonadPlus, mzero)
import Data.List (intercalate)
import Data.Maybe (isJust, fromJust)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Typeable
import PortCapability
import System.IO (hPrint, stderr)
import Text.StringTemplate (STGroup)

{-# ANN module "HLint: ignore Use camelCase" #-}

--------------------------------------------------------------------------
class (Eq a, Typeable a) => AlgoLike a where
    getName :: a -> String
    getPortCap :: a -> PortCapability
    getDeps :: a -> [PortCapability]
    getLvl :: a -> Int
    emitAlgoTxt :: a -> String -> STGroup String -> String

data Algo = forall a . (AlgoLike a) => MkAlgo a deriving (Typeable)

instance Eq Algo where
    (MkAlgo a) == (MkAlgo a') = Just a == cast a'

instance AlgoLike Algo where
    getName (MkAlgo a) = getName a
    getPortCap (MkAlgo a) = getPortCap a
    getDeps (MkAlgo a) = getDeps a
    getLvl (MkAlgo a) = getLvl a
    emitAlgoTxt (MkAlgo a) = emitAlgoTxt a

instance Show Algo where
    show a = show (getPortCap a)
               ++ (if getName a == _BASE_name then "" else getName a)
               ++ (if null (getDeps a) then "" else "::" ++ show (getDeps a))
               ++ ("(" ++ show (getLvl a) ++ ")")

instance Ord Algo where
    a `compare` a'
        = compare (getPortCap a, getName a, getLvl a, getDeps a)
                  (getPortCap a', getName a', getLvl a', getDeps a')

instance NFData Algo where
    rnf a = rnf (getName a, getPortCap a, getLvl a, getDeps a) `seq` ()

toAlgo :: AlgoLike a => a -> Algo
toAlgo = MkAlgo
--------------------------------------------------------------------------

timeIt :: (NFData a) => IO a -> IO a
timeIt io = do
    t1 <- getCurrentTime
    a <- io
    t2 <- a `deepseq` getCurrentTime
    hPrint stderr $ diffUTCTime t2 t1
    return a

ceil :: Int -> Int -> Int
ceil n m = n `div` m + (if (n `rem` m) == 0 then 0 else 1)

when :: (MonadPlus m) => Bool -> m a -> m a
when p v = if p then v else mzero

-- a = AlgoSpecific, b = AlgoRegistry
alg_fn1 :: (Int -> Maybe a) -> (b -> a -> [Algo]) -> b -> [Algo]
alg_fn1 f g b
    = concat
        $ takeWhile (not.null)
        $ map (g b . fromJust)
        $ takeWhile isJust
        $ map f [1..]

alg_fn2 :: (Int -> Int -> Maybe a) -> (b -> a -> [Algo]) -> b -> [Algo]
alg_fn2 f g b
    = concat
        $ takeWhile (not.null)
        $ map (\i -> alg_fn1 (f i) g b) [1..]

alg_fn3 :: (Int -> Int -> Int -> Maybe a) -> (b -> a -> [Algo]) -> b -> [Algo]
alg_fn3 f g b
    = concat
        $ takeWhile (not.null)
        $ map (\i -> alg_fn2 (f i) g b) [1..]

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
            (pc_ORed . _filter_redundant_pcs)

_dot_prod :: PCOR -> PCOR -> PCOR
_dot_prod (r, w, rws) (r', w', r'w's)
    = (r+r', w+w', [(r,w'), (r',w)]
                   ++ [(r+a,b)   | (a,b) <- r'w's]
                   ++ [(c,w+d)   | (c,d) <- r'w's]
                   ++ [(e+r',f)  | (e,f) <- rws]
                   ++ [(g,h+w')  | (g,h) <- rws])

_filter_redundant :: (a -> a -> Bool) -> [a] -> [a]
_filter_redundant covers_fn = fr []
    where fr goods [] = goods
          fr goods (a:rems)
              | any (`covers_fn` a) $ goods ++ rems = fr goods rems
              | otherwise = fr (a:goods) rems

_filter_redundant_pcs :: [PortCapability] -> [PortCapability]
_filter_redundant_pcs = _filter_redundant covers
