module AlgoRegistry where

import AlgoUtils
import qualified Data.HashMap.Lazy as HM (HashMap, empty, insert, lookupDefault, elems, map, toList)
import PortCapability

data AlgoRegistry = Registry (HM.HashMap PortCapability [Algo])

getFromReg :: PortCapability -> AlgoRegistry -> [(Int, PortCapability)]
getFromReg reqPc (Registry reg)
    = concatMap (\(lvl, pcs) -> map (\pc -> (lvl, pc)) pcs)
        $ HM.toList
        $ HM.map _minimumPcs
        $ foldr (\(pc, algs) hm ->
                    let lvl = _minimumLvl algs
                    in HM.insert lvl (pc:HM.lookupDefault [] lvl hm) hm)
            HM.empty
        $ filter (\(pc, _) -> pc `covers` reqPc)
        $ HM.toList reg

_minimumLvl :: [Algo] -> Int
_minimumLvl algs = minimum $ map getLvl algs

_minimumPcs :: [PortCapability] -> [PortCapability]
_minimumPcs = foldr (_fitMin covers) []

emptyReg :: AlgoRegistry
emptyReg = Registry HM.empty

addAlgToReg :: Algo -> AlgoRegistry -> AlgoRegistry
addAlgToReg alg (Registry hm)
    = let pc = getPortCap alg
          existingAlgs = HM.lookupDefault [] pc hm
          newAlgs = _fitMin (flip algCovers) alg existingAlgs
      in Registry $ HM.insert pc newAlgs hm

getAlgos :: AlgoRegistry -> [Algo]
getAlgos (Registry hm) = concat $ HM.elems hm

_fitMin :: (a -> a -> Bool) -> a -> [a] -> [a]
_fitMin grtEq p ps = _fitMin' p ps []
    where _fitMin' x [] min' = x:min'
          _fitMin' x (y:ys) min'
            | y `grtEq` x = _fitMin' x ys min'
            | x `grtEq` y = (y:ys) ++ min'
            | otherwise = _fitMin' x ys (y:min')
