{-# Language NamedFieldPuns #-}

module Main where

import System.Environment (getArgs)

import Symbex
import Print

import Dappsys.Weth

import Data.Generics.Uniplate.Data

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as B8

import Text.Printf

showPath :: Path -> IO ()
showPath (Path x (State { storage }) o) = do
  putStrLn $ "** Conditions"
  mapM_ putStrLn (map (display . rewrite optimize) x)
  putStrLn $ "\n** Outcome " ++ show o
  -- putStr "Stack:   "
  -- putStrLn $ "(" ++ intercalate " " (map (display . rewrite optimize) stack) ++ ")"
  -- putStr "Memory:  "
  -- putStrLn (display memory)
  -- putStrLn "\nMemory:  "
  -- putStrLn (display (rewriteBi (optimize :: AValue -> Maybe AValue) memory))
  -- putStr "Storage: "
  -- putStrLn (display storage)
  putStrLn "\n** Storage"
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
      putStrLn $ "* Path " ++ show (i :: Int) ++ "\n"
      showPath x

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    ["--bytecode", "weth"] -> do
      mapM_ (printf "%02x") (compile weth)
      putStrLn ""
    _ -> do
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
