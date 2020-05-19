{-# LANGUAGE LambdaCase #-}
module Main where

import Language.Carry.M
import Language.Carry.Parser

import Codec.Phaser
import Data.Foldable
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  traverse_ (\filename -> parseFile sourceFile filename >>= \case
     Right _ -> putStrLn $ "Parser reports success for `" ++ filename ++ "`"
     Left fe -> do
       putStrLn "Parse error:"
       traverse_ (\ ~(p,pe) -> do
         putStrLn $ show p ++ ":"
         traverse_ (\e -> putStrLn $ "  " ++ e) pe
        ) fe
    ) args
