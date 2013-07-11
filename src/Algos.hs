{-# LANGUAGE DeriveDataTypeable #-}
module Algos where

import A_BASE
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
import Data.List (sort, intercalate)
import Data.Maybe (fromJust)
--import Debug.Trace (trace)
import PortCapability
import System.Environment
--import System.IO.Unsafe (unsafePerformIO)
import Text.StringTemplate (directoryGroup)

{-# ANN module "HLint: ignore Use camelCase" #-}

_basePcs :: [PortCapability]
_basePcs = map fromJust [pc_nRW 1, pc_nRmW 1 1, pc_nRW 2, pc_nRmW 2 2]

_baseAlgs :: [Algo]
_baseAlgs = map getBaseAlgo _basePcs

_baseReg :: AlgoRegistry
_baseReg = foldr addAlgToReg emptyReg _baseAlgs

_algFns :: [AlgoRegistry -> [Algo]]
_algFns = concat [_MT_Alg, _RL_Alg, _XR_Alg, _REP_Alg]

_iter :: AlgoRegistry -> AlgoRegistry
_iter reg = let algss = map (\fn -> fn reg) _algFns
                            `using` evalList (rpar `dot` rdeepseq)
            in foldr addAlgToReg reg $ concat algss

_generateAlgos :: Int -> [Algo]
_generateAlgos n = getAlgos $ last $ take (n+1) $ iterate _iter _baseReg

_printAlgoSigs :: [Algo] -> IO()
_printAlgoSigs
    = mapM_ print
        . sort
        . filter (\alg -> getName alg `notElem` [_BASE_name, _REP_name])

_printAlgoTxt :: [Algo] -> FilePath -> IO()
_printAlgoTxt algs file = do
    templates <- directoryGroup "templates"
    writeFile file
      $ intercalate "\n\n----------------------------------\n\n"
      $ zipWith (\alg n -> emitAlgoTxt alg (show n) templates)
        (sort algs) ([1..] :: [Int])
            
----------------------------------------------------------------------------------------------

main :: IO()
main = do
    args <- getArgs
    let n = read $ head args
        allAlgos = _generateAlgos n
    _printAlgoSigs allAlgos
    _printAlgoTxt allAlgos "all_algos.txt"
    
