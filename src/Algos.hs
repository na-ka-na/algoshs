{-# LANGUAGE DeriveDataTypeable #-}
module Algos where

import A_MT
import A_REP
import A_RL
import A_XR
import AlgoRegistry
import AlgoUtils
import Constants
--import Control.Concurrent (threadDelay)
--import Control.DeepSeq (deepseq)
--import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (using, rdeepseq, rpar, evalList, dot)
import Data.List (sort)
import Data.Maybe (fromJust)
--import Debug.Trace (trace)
import Data.Typeable
import PortCapability
import System.Environment
--import System.IO.Unsafe (unsafePerformIO)

{-# ANN module "HLint: ignore Use camelCase" #-}

_basePcs :: [PortCapability]
_basePcs = map fromJust [pc_nRW 1, pc_nRmW 1 1, pc_nRW 2, pc_nRmW 2 2]

data A_BASE = A_BASE PortCapability deriving (Eq, Typeable)
instance AlgoLike A_BASE where
    getName _ = _BASE_name
    getPortCap (A_BASE pc) = pc
    getDeps _ = []
    getLvl _ = 0

_baseAlgs :: [Algo]
_baseAlgs = map (toAlgo . A_BASE) _basePcs

_baseReg :: AlgoRegistry
_baseReg = foldr addAlgToReg emptyReg _baseAlgs

_algFns :: [AlgoRegistry -> [Algo]]
_algFns = concat [_MT_Alg, _RL_Alg, _XR_Alg, _REP_Alg]

_iter :: AlgoRegistry -> AlgoRegistry
_iter reg = let algss = map (\fn -> fn reg) _algFns
                            `using` evalList (rpar `dot` rdeepseq)
            in foldr addAlgToReg reg $ concat algss

_printReg :: AlgoRegistry -> IO()
_printReg reg
    = mapM_ print
        $ sort
        $ filter (\alg -> getName alg `notElem` [_BASE_name, _REP_name])
        $ getAlgos reg

_printAlgs :: Int -> IO()
_printAlgs n = _printReg $ last $ take (n+1) $ iterate _iter _baseReg
----------------------------------------------------------------------------------------------

main :: IO()
main = do
    args <- getArgs
    _printAlgs (read $ head args)


