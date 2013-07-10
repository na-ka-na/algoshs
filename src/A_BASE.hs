{-# LANGUAGE DeriveDataTypeable #-}
module A_BASE (getBaseAlgo) where

import AlgoUtils
import Constants
import Data.Typeable
import PortCapability
import Text.StringTemplate (getStringTemplate, render, setManyAttrib)

{-# ANN module "HLint: ignore Use camelCase" #-}

getBaseAlgo :: PortCapability -> Algo
getBaseAlgo pc = toAlgo $ A_BASE pc

data A_BASE = A_BASE PortCapability deriving (Eq, Typeable)
instance AlgoLike A_BASE where
    getName _ = _BASE_name
    getPortCap (A_BASE pc) = pc
    getDeps _ = []
    getLvl _ = 0
    emitAlgoTxt (A_BASE bank) _ templates
        = let Just t = getStringTemplate "REP" templates
              attrs = [("bank", show bank)]
          in render $ setManyAttrib attrs t
