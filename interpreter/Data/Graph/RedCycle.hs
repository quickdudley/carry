{-|
Module:     Data.Graph.RedCycle
Copyright:  Jeremy List
License:    BSD-3
Maintainer: quick.dudley@gmail.com

The interpreter and compiler need to allow mutually dependent functions and
modules but since mutually dependent splices are impossible it's best to detect
them as early as possible. This can be done with a variation of the
Floyd-Warshall algorithm. 
-}
module Data.Graph.RedCycle (
  EdgeColour(..),
  hasRedCycles
 ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.List (foldl', sort)

data EdgeColour = Red | Black deriving (Eq,Ord,Show,Read)

-- | Given a directed graph of coloured edges: determine whether or not there
-- is any cycle which contains a red edge
hasRedCycles :: [(Int,Int,EdgeColour)] -> Bool
hasRedCycles [] = False
hasRedCycles el = let
  (b,t) = foldl' (\(b0,t0) (f,s,_) -> let
    [b1,_,_,t1] = sort [b0,f,s,t0]
    in b1 `seq` t1 `seq` (b1,t1)
   ) (let ((i0,j0,_):_) = el; [b0,t0] = sort [i0,j0] in (b0,t0)) el
  go :: ST s Bool
  go = do
    arr <- newArray ((b,b),(t,t)) False :: ST s (STUArray s (Int,Int) Bool)
    forM_ el $ \(f,s,c) -> writeArray arr (f,s) True
    forM_ [b .. t] $ \i -> forM_ [b .. t] $ \j -> forM_ [b .. t] $ \k -> do
      ji <- readArray arr (j,i)
      when ji $ do
        ik <- readArray arr (i,k)
        when ik $ writeArray arr (j,k) True
    foldr (\(f,s,c) r -> case c of
      Red -> do
        y <- readArray arr (f,f)
        if y
          then return True
          else r
      _ -> r
     ) (return False) el
  in runST go
