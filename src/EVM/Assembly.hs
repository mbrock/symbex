{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language NamedFieldPuns #-}

module EVM.Assembly where

import Prelude hiding (not, and, or, exp, return, div, log)

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Bits
import Data.List (unfoldr, mapAccumL)
import Data.Word
import Text.Printf

import GHC.Generics
import GHC.Stack

import Control.Monad.State (get, modify, execState)
import qualified Control.Monad.State as Monad

type Assembler i a = Monad.State ([i], Integer) a
type Assembly = Assembler Instr ()

bytecode :: Assembly -> String
bytecode = concatMap (printf "%02x") . compile . assemble

emit :: CallStack -> Instr' -> Assembly
emit cs x =
  modify $
    \(xs, i) ->
      (Instr Nothing cs x : xs, succ i)

assemble :: Assembly -> [Instr]
assemble x = reverse y
  where (y, _) = execState x ([], 0)

as :: String -> Assembler Instr a -> Assembler Instr a
as s m =
  do r <- m
     modify $
       \(Instr _ cs x : xs, i) -> (Instr (Just s) cs x : xs, i)
     pure r

(?) :: Assembler Instr a -> String -> Assembler Instr a
(?) = flip as

infix 0 ?

data Label = Label Integer
  deriving (Show, Generic)

label :: HasCallStack => Assembler Instr Label
label = do
  (_, i) <- get
  emit callStack Jumpdest
  pure (Label i)

data Instr = Instr
  { instrAnnotation :: Maybe String
  , instrCallstack :: CallStack
  , op :: Instr'
  } deriving (Show, Generic)

data Instr'
  = Push Integer
  | PushLabel Label
  | Dup Int
  | Pop
  | Swap Int
  | Log Int
  | Caller
  | Eq
  | Jumpi
  | Stop
  | Mstore
  | Mstore8
  | Mload
  | Calldatasize
  | Gt
  | Lt
  | Calldataload
  | Sload
  | Sstore
  | Byte
  | Calldatacopy
  | Msize
  | Keccak256
  | Timestamp
  | Gaslimit
  | Call
  | Sub
  | Add
  | Not
  | And
  | Or
  | Exp
  | Callvalue
  | Iszero
  | Div
  | Revert
  | Return
  | Jump
  | Jumpdest
  deriving (Show, Generic)

instance ToJSON Label
instance ToJSON Instr'
instance ToJSON Instr where
  toJSON (Instr ann _ op) =
    object
      [ "instrAnnotation" .= toJSON ann
      , "op" .= toJSON op
      ]

push :: HasCallStack => Integer -> Assembly; push = emit callStack . Push
refer :: HasCallStack => Label -> Assembly; refer = emit callStack . PushLabel
dup :: HasCallStack => Int -> Assembly; dup = emit callStack . Dup
mstore :: HasCallStack => Assembly; mstore = emit callStack Mstore
mstore8 :: HasCallStack => Assembly; mstore8 = emit callStack Mstore8
mload :: HasCallStack => Assembly; mload = emit callStack Mload
msize :: HasCallStack => Assembly; msize = emit callStack Msize
pop :: HasCallStack => Assembly; pop = emit callStack Pop
eq :: HasCallStack => Assembly; eq = emit callStack Eq
stop :: HasCallStack => Assembly; stop = emit callStack Stop
swap :: HasCallStack => Int -> Assembly; swap = emit callStack . Swap
caller :: HasCallStack => Assembly; caller = emit callStack Caller
jumpi :: HasCallStack => Assembly; jumpi = emit callStack Jumpi
sload :: HasCallStack => Assembly; sload = emit callStack Sload
sstore :: HasCallStack => Assembly; sstore = emit callStack Sstore
byte :: HasCallStack => Assembly; byte = emit callStack Byte
calldatasize :: HasCallStack => Assembly; calldatasize = emit callStack Calldatasize
calldataload :: HasCallStack => Assembly; calldataload = emit callStack Calldataload
calldatacopy :: HasCallStack => Assembly; calldatacopy = emit callStack Calldatacopy
timestamp :: HasCallStack => Assembly; timestamp = emit callStack Timestamp
gaslimit :: HasCallStack => Assembly; gaslimit = emit callStack Gaslimit
gt :: HasCallStack => Assembly; gt = emit callStack Gt
lt :: HasCallStack => Assembly; lt = emit callStack Lt
add :: HasCallStack => Assembly; add = emit callStack Add
sub :: HasCallStack => Assembly; sub = emit callStack Sub
not :: HasCallStack => Assembly; not = emit callStack Not
call :: HasCallStack => Assembly; call = emit callStack Call
keccak256 :: HasCallStack => Assembly; keccak256 = emit callStack Keccak256
exp :: HasCallStack => Assembly; exp = emit callStack Exp
and :: HasCallStack => Assembly; and = emit callStack And
or :: HasCallStack => Assembly; or = emit callStack Or
div :: HasCallStack => Assembly; div = emit callStack Div
return :: HasCallStack => Assembly; return = emit callStack Return
iszero :: HasCallStack => Assembly; iszero = emit callStack Iszero
jump :: HasCallStack => Assembly; jump = emit callStack Jump
revert :: HasCallStack => Assembly; revert = emit callStack Revert
callvalue :: HasCallStack => Assembly; callvalue = emit callStack Callvalue
log :: HasCallStack => Int -> Assembly; log = emit callStack . Log

unroll :: Integer -> [Word8]
unroll = unfoldr f
  where
    f 0 = Nothing
    f i = Just (fromIntegral i, i `shiftR` 8)

pad :: Int -> a -> [a] -> [a]
pad n x xs =
  if length xs > n
  then error "too big"
  else replicate (length xs - n) x ++ xs

compile :: [Instr] -> [Word8]
compile xs =
  let
    pass1 :: [Pass1]
    pass1 = map (compile1 . op) xs

    pass2 :: [Int]
    pass2 = snd (mapAccumL f 0 pass1)
      where
        f i (Bytecode code) = (i + length code, i)
        f i (Unresolved _)  = (i + 3, i)

    stupid :: Pass1 -> [Word8]
    stupid (Bytecode code) = code
    stupid (Unresolved i) =
      0x62 : pad 2 0 (unroll (fromIntegral (pass2 !! i)))
  in
    concatMap stupid pass1

data Pass1 = Bytecode [Word8] | Unresolved Int

compile1 :: Instr' -> Pass1
compile1 = \case
  Push x ->
    let
      bytes = unroll x
      count = length bytes
    in
      if count == 0
      then Bytecode [0x60, 0]
      else Bytecode $ fromIntegral (0x60 + count - 1) : bytes
  PushLabel (Label x) -> Unresolved (fromIntegral x)
  Dup x -> Bytecode [fromIntegral $ 0x80 + x - 1]
  Swap x -> Bytecode [fromIntegral $ 0x90 + x - 1]
  Log x -> Bytecode [fromIntegral $ 0xa0 + x]
  Pop -> Bytecode [0x50]
  Caller -> Bytecode [0x33]
  Eq -> Bytecode [0x14]
  Jumpi -> Bytecode [0x57]
  Stop -> Bytecode [0x00]
  Mstore -> Bytecode [0x52]
  Mstore8 -> Bytecode [0x53]
  Mload -> Bytecode [0x51]
  Calldatasize -> Bytecode [0x36]
  Gt -> Bytecode [0x11]
  Lt -> Bytecode [0x10]
  Calldataload -> Bytecode [0x35]
  Sload -> Bytecode [0x54]
  Sstore -> Bytecode [0x55]
  Byte -> Bytecode [0x1a]
  Calldatacopy -> Bytecode [0x37]
  Msize -> Bytecode [0x59]
  Keccak256 -> Bytecode [0x20]
  Timestamp -> Bytecode [0x42]
  Gaslimit -> Bytecode [0x45]
  Call -> Bytecode [0xf1]
  Sub -> Bytecode [0x03]
  Add -> Bytecode [0x01]
  Not -> Bytecode [0x19]
  And -> Bytecode [0x16]
  Or -> Bytecode [0x17]
  Exp -> Bytecode [0x0a]
  Callvalue -> Bytecode [0x34]
  Iszero -> Bytecode [0x15]
  Div -> Bytecode [0x04]
  Revert -> Bytecode [0xfd]
  Return -> Bytecode [0xf3]
  Jump -> Bytecode [0x56]
  Jumpdest -> Bytecode [0x5b]
