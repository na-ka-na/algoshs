module Algo (Algo(..)) where

import Constants
import Data.Hashable (Hashable, hash, hashWithSalt)
import PortCapability

{-# ANN module "HLint: ignore Use camelCase" #-}

-- name pc lvl pcs
data Algo = Algo String PortCapability Int [PortCapability] deriving (Eq, Ord)

instance Hashable Algo where
    hashWithSalt salt (Algo name pc lvl pcs)
        = foldr hashWithSalt salt $ [hash name, hash pc, lvl] ++ map hash pcs

instance Show Algo where
    show (Algo name pc _ pcs)
        = show pc ++ (if name==_BASE_name then "" else name)
          ++ (if null pcs then "" else "::" ++ show pcs)
