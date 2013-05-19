module Algo (Algo(..)) where

import Constants
import Control.DeepSeq (NFData(..))
import Data.Hashable (Hashable, hash, hashWithSalt)
import PortCapability

{-# ANN module "HLint: ignore Use camelCase" #-}

-- name pc lvl pcs
data Algo = Algo String PortCapability Int [PortCapability] deriving (Eq)

instance Hashable Algo where
    hashWithSalt salt (Algo name pc lvl pcs)
        = foldr hashWithSalt salt $ [hash name, hash pc, lvl] ++ map hash pcs

instance Show Algo where
    show (Algo name pc lvl pcs)
        = show pc ++ (if name==_BASE_name then "" else name)
          ++ (if null pcs then "" else "::" ++ show pcs)
          ++ ("(" ++ show lvl ++ ")")

instance Ord Algo where
    (Algo name pc lvl pcs) `compare` (Algo name' pc' lvl' pcs')
        = compare (pc, name, lvl, pcs) (pc', name', lvl', pcs')

instance NFData Algo where
    rnf (Algo name pc lvl pcs) = rnf (name, pc, lvl, pcs) `seq` ()
