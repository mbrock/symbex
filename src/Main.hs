{-# Language NamedFieldPuns #-}

module Main where

import System.Environment (getArgs)

import Symbex
import Print

import Data.List (intercalate)
import Data.Generics.Uniplate.Data

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as B8

showPath :: Path -> IO ()
showPath (Path x (State { stack, memory, storage }) o) = do
  putStrLn $ "Conditions:"
  mapM_ putStrLn (map (display . rewrite optimize) x)
  putStrLn ""
  putStrLn $ "Outcome: " ++ show o
  putStr "Stack:   "
  putStrLn $ "(" ++ intercalate " " (map (display . rewrite optimize) stack) ++ ")"
  -- putStr "Memory:  "
  -- putStrLn (display memory)
  putStrLn "\nMemory:  "
  putStrLn (display (rewriteBi (optimize :: AValue -> Maybe AValue) memory))
  -- putStr "Storage: "
  -- putStrLn (display storage)
  putStrLn "\nStorage: "
  putStrLn (display (rewriteBi (optimize :: AValue -> Maybe AValue) storage))
  putStrLn ""

emptyState :: State
emptyState = State
  { stack = []
  , pc = 0
  , memory = Null
  , storage = Null
  }

run :: Code -> [Path]
run code = step code emptyState

showPaths :: [Path] -> IO ()
showPaths = mapM_ f . zip [1..]
  where
    f (i, x) = do
      putStrLn "================"
      putStrLn $ "Path " ++ show (i :: Int) ++ ". "
      putStrLn "================\n"
      showPath x

main :: IO ()
main = do
  xs <- getArgs
  let
    (json, xs') =
      case xs of
        ("--json":_) -> (True, tail xs)
        _ -> (False, xs)
    thing =
      case xs' of
        ["weth"] -> weth
        ["multisig"] -> multisig2
        _ -> error "wtf"

  if json
    then
      B8.putStrLn . encode $ step' thing emptyState
    else
      showPaths (run thing)
