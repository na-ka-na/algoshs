{-# LANGUAGE DeriveDataTypeable #-}
module A_REP (_REP_Alg) where

import AlgoRegistry
import AlgoUtils
import Constants
import Data.Typeable
import PortCapability
import Text.StringTemplate (getStringTemplate, render, setManyAttrib)

{-# ANN module "HLint: ignore Use camelCase" #-}

type Bank1 = PortCapability
type Bank2 = PortCapability
type Lvl = Int

-- REP pc1 pc2 where pc1 >= nRmW and pc2 >= n'RmW => (n+n')RmW
_REP1 :: Int -> Int -> Int -> Maybe (Bank1, Bank2, PortCapability)
_REP1 n n' m = do
    bank1 <- pc_nRmW n m
    bank2 <- pc_nRmW n' m
    algo <- pc_nRmW (n+n') m
    return (bank1, bank2, algo)

data A_REP = A_REP PortCapability Lvl Bank1 Bank2 deriving (Eq, Typeable)
instance AlgoLike A_REP where
    getName _ = _REP_name
    getPortCap (A_REP pc _ _ _) = pc
    getDeps _ = []
    getLvl (A_REP _ lvl _ _) = lvl
    emitAlgoTxt (A_REP algo _ bank1 bank2) idSuffix templates
        = let Just t = getStringTemplate "REP" templates
              attrs = [("idSuffix", idSuffix),
                       ("algo", show algo),
                       ("bank1", show bank1),
                       ("bank2", show bank2)]
          in render $ setManyAttrib attrs t

_to_A_REP :: AlgoRegistry -> (Bank1, Bank2, PortCapability) -> [Algo]
_to_A_REP ar (bank1, bank2, algo) = do
    (bank1Lvl, bank1') <- getFromReg bank1 ar
    (bank2Lvl, bank2') <- getFromReg bank2 ar
    let algoLvl = max bank1Lvl bank2Lvl
    return $ toAlgo $ A_REP algo algoLvl bank1' bank2'

_REP_Alg :: [AlgoRegistry -> [Algo]]
_REP_Alg
    = map (\f -> f _to_A_REP)
        [alg_fn3 _REP1]
