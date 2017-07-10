{-|
Module:     Data.Graph.RedCycle
Copyright:  Jeremy List
License:    BSD-3
Maintainer: quick.dudley@gmail.com

The interpreter and compiler need to allow mutually dependent functions and
modules but since mutually dependent splices are impossible it's best to detect
them as early as possible. The current version uses a variation on Tarjan's
strongly connected components algorithm.
-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Graph.RedCycle (
  EdgeColour(..),
  hasRedCycles
 ) where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.ST
import Control.Monad.State
import Data.Array.ST
import Data.List (foldl', sort)
import qualified Data.Map as M
import Data.STRef

data EdgeColour = Red | Black deriving (Eq,Ord,Show,Read)

-- | Given a directed graph of coloured edges: determine whether or not there
-- is any cycle which contains a red edge
hasRedCycles :: [(Int,Int,EdgeColour)] -> Bool
hasRedCycles [] = False
hasRedCycles  g = runST $ do
  let
    (b,t) = foldl' (\(b0,t0) (f,s,_) -> let
      [b1,_,_,t1] = sort [b0,f,s,t0]
      in b1 `seq` t1 `seq` (b1,t1)
     ) (let ((i0,j0,_):_) = g; [b0,t0] = sort [i0,j0] in (b0,t0)) g
  remainingEdges <- newSTRef g
  arr <- newArray (b,t) [] :: ST s (STArray s Int [Int])
  stk <- newArray (b,t) False :: ST s (STUArray s Int Bool)
  s <- newSTRef [] :: ST s (STRef s [Int])
  idxs <- newArray (b,t) Nothing :: ST s (STArray s Int (Maybe Int))
  idx <- newSTRef 0 :: ST s (STRef s Int)
  lowlink <- newArray (b,t) maxBound :: ST s (STUArray s Int Int)
  forM_ g $ \(s,t,_) -> readArray arr s >>= writeArray arr s . (t:)
  let
    setLink v w = lift $ do
      vl <- readArray lowlink v
      wl <- readArray lowlink w
      when (wl < vl) $ writeArray lowlink v wl
    strongConnect v = do
      i' <- lift $ readSTRef idx
      lift $ writeArray idxs v (Just i')
      lift $ writeArray lowlink v i'
      lift $ modifySTRef idx (+ 1)
      lift $ modifySTRef s (v:)
      lift $ writeArray stk v True
      e <- lift $ readArray arr v
      forM_ e $ \w -> do
        i <- lift $ readArray idxs w
        case i of
          Nothing -> do
            strongConnect w
            setLink v w
          Just wi -> do
            o <- lift $ readArray stk w
            when o $ setLink v w
      vl <- lift $ readArray lowlink v
      when (i' == vl) $ do
        cc <- lift (newArray (b,t) False :: ST s (STUArray s Int Bool))
        let
          poploop = do
            s' <- readSTRef s
            case s' of
              (w:r) -> do
                writeSTRef s r
                writeArray stk w False
                writeArray cc w True
                when (w /= v) poploop
              _ -> return ()
        lift $ poploop
        cre <- lift $ readSTRef remainingEdges <*
          writeSTRef remainingEdges []
        forM_ cre $ \e@(ns,nt,c) -> do
          cs <- lift $ (,) <$> readArray cc ns <*> readArray cc nt
          case cs of
            (True,True) -> case c of
              Red -> ContT $ const $ return True
              Black -> return ()
            (False,False) -> lift $ modifySTRef remainingEdges (e:)
            _ -> return ()
  runContT (forM_ [b .. t] strongConnect) (const $ return False)

renumberNodes :: [(Int,Int,EdgeColour)] -> [(Int,Int,EdgeColour)]
renumberNodes g = flip evalState (M.empty,1) $ forM g $ \(s,t,c) -> do
  s' <- renumber s
  t' <- renumber t
  return (s',t',c)
 where
  renumber n = get >>= \(m,a) -> case M.lookup n m of
    Just n' -> return n'
    Nothing -> put (M.insert n a m, a + 1) >> return a
