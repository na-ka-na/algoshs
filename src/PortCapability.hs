module PortCapability
(
  PortCapability(..)
, portCap
, covers
) where

import Control.Exception (assert)
import Data.Hashable (Hashable, hash, hashWithSalt)
import qualified Data.HashSet as HS (HashSet, fromList, toList, unions, insert, empty, null, difference)
import qualified Data.HashMap.Strict as HM (HashMap, empty, insert, lookup)
import Data.List ()
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
--import Debug.Trace (trace)
import qualified PortCapParser as PP
import System.IO.Unsafe (unsafePerformIO)

{-# ANN module "HLint: ignore Use camelCase" #-}
-------------------------------------------------------------------------

memoize1 :: (Eq k, Hashable k, Show k, Show v) => (k -> v) -> k -> v
memoize1 f = unsafePerformIO $ do             
                 cacheRef <- newIORef HM.empty
                 return $ newf cacheRef f

-- SCC (set cost center)
newf :: (Eq k, Hashable k) => IORef(HM.HashMap k v) -> (k -> v) -> k -> v
newf cacheRef f k
    = unsafePerformIO $ do
          cache <- readIORef cacheRef
          case HM.lookup k cache of
              Just v -> return v
              Nothing -> do let v = f k
                            modifyIORef cacheRef (HM.insert k v)
                            return v

-------------------------------------------------------------------------

-- r rw w ru c a
data PC = PC Int Int Int Int Int Int deriving (Eq, Ord, Show)
instance Hashable PC where
    hashWithSalt salt (PC r rw w ru c a) = salt * (r+rw+w+ru+c+a)

newpc :: Int -> Int -> Int -> Int -> Int -> Int -> PC
newpc r rw w ru c a = PC (assert (r>=0) r) (assert (rw>=0) rw) (assert (w>=0) w)
                         (assert (ru>=0) ru) (assert (c>=0) c) (assert (a>=0) a)

toPortCapString :: PC -> String
toPortCapString (PC r rw w ru c a) =
    let pcStr = concat $ zipWith (\p pstr -> if p==0 then "" else show p ++ pstr)
                         [ r,   rw,   w,   ru,   c,   a]
                         ["R", "RW", "W", "RU", "C", "A"]
    in (if pcStr=="" then "NOOP" else pcStr)

numLines :: PC -> Int
numLines (PC r rw w ru c a) = r+rw+w+ru+c+a

toType1s :: PC -> HS.HashSet PC
toType1s pc@(PC r rw w ru c a) =
    let type1s = [PC r' 0 w' ru' c' a' | r' <- [0..(r+rw)], w' <- [0..(w+rw)]
                 , ru' <- [0..ru], c' <- [0..c], a' <- [0..a]
                 , (r'+w') <= (r+w+rw), (r'+w'+ru'+c'+a') <= numLines pc]
    in HS.fromList type1s

-------------------------------------------------------------------------

-- orig hsh pcs simple type1s subports
data PortCapability = PortCapability String Int [PC] Bool (HS.HashSet PC) (HS.HashSet PortCapability)

instance Eq PortCapability where
    PortCapability orig _ _ simple type1s _ == PortCapability orig' _ _ simple' type1s' _
        | simple && simple' = orig == orig'
        | otherwise = type1s == type1s'

instance Hashable PortCapability where
    hashWithSalt salt (PortCapability _ hsh _ _ _ _) = salt * hsh

_hashType1s :: HS.HashSet PC -> Int
_hashType1s = foldr (hashWithSalt . hash) 17 . HS.toList

instance Show PortCapability where
    show (PortCapability orig _ _ _ _ _) = orig

instance Ord PortCapability where
    compare pc1@(PortCapability orig1 _ _ _ _ _) pc2@(PortCapability orig2 _ _ _ _ _)
        = compare (pcForm pc1, orig1) (pcForm pc2, orig2)

portCap :: String -> PortCapability
portCap = memoize1
          (\orig -> let pps = PP.parsePortCap orig
                        pcs = map (\(PP.PC r rw w ru c a) -> newpc r rw w ru c a)
                              (assert (not.null $ pps) pps)
                        simple = (length pcs == 1) && (\(PC _ rw _ _ _ _:_) -> rw == 0) pcs
                        type1s = HS.unions $ map toType1s pcs
                        hsh = _hashType1s type1s
                        subports = foldr (\pc subs -> HS.insert (portCap $ toPortCapString pc) subs)
                                     HS.empty $ HS.toList type1s
                    in PortCapability orig hsh pcs simple type1s subports)

covers :: PortCapability -> PortCapability -> Bool
(PortCapability _ _ [PC r 0 w ru c a] True _ _) `covers` (PortCapability _ _ [PC r' 0 w' ru' c' a'] True _ _)
    = (r >= r') && (w >= w') && (ru >= ru') && (c >= c') && (a >= a')
(PortCapability _ _ _ _ _ subports) `covers` (PortCapability _ _ _ _ _ subports')
    = HS.null $ HS.difference subports' subports

-------------------------------------------------------------------------

data PC_FORM = PC_NOOP | PC_nR | PC_nW | PC_nRW | PC_nRmW | PC_nRmRWpW | PC_nRormW
             | PC_nC | PC_nA | PC_nCormA | PC_OTHER
             deriving (Eq, Ord, Show, Read, Enum)

pcForm :: PortCapability -> PC_FORM
pcForm (PortCapability _ _ [PC r rw w ru c a] _ _ _) -- length pcs == 1
    | lines' == 0        = PC_NOOP
    | lines' == r        = PC_nR
    | lines' == w        = PC_nW
    | lines' == rw       = PC_nRW
    | lines' == (r+w)    = PC_nRmW
    | lines' == (r+rw+w) = PC_nRmRWpW
    | lines' == c        = PC_nC
    | lines' == a        = PC_nA
    where lines' = r + rw + w + ru + c + a

pcForm (PortCapability _ _ [PC r rw w ru c a, PC r' rw' w' ru' c' a'] _ _ _) -- length pcs == 2
    | (lines' == r) && (lines'' == w') = PC_nRormW
    | (lines' == c) && (lines'' == a') = PC_nCormA
    where lines'  = r + rw + w + ru + c + a
          lines'' = r' + rw' + w' + ru' + c' + a'

pcForm _ = PC_OTHER


