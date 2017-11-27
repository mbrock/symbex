{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language NamedFieldPuns #-}

module EVM.Assembly where

import Prelude hiding (not, and, or, exp, return, div, log)

import Control.Monad.State (get, modify, execState)
import qualified Control.Monad.State as Monad
import GHC.Generics
import Data.Aeson (ToJSON ())
import Data.Word
import Data.List (unfoldr, mapAccumL)
import Data.Bits
import Text.Printf

type Assembler i a = Monad.State ([i], Integer) a
type Assembly = Assembler Instr ()

bytecode :: Assembly -> String
bytecode = concatMap (printf "%02x") . compile . assemble

emit :: Instr' -> Assembly
emit x =
  modify $
    \(xs, i) ->
      (Instr Nothing x : xs, succ i)

assemble :: Assembly -> [Instr]
assemble x = reverse y
  where (y, _) = execState x ([], 0)

as :: String -> Assembler Instr a -> Assembler Instr a
as s m =
  do r <- m
     modify $ \(Instr _ x : xs, i) -> (Instr (Just s) x : xs, i)
     pure r

(?) :: Assembler Instr a -> String -> Assembler Instr a
(?) = flip as

infix 0 ?

data Label = Label Integer
  deriving (Show, Generic)

label :: Assembler Instr Label
label = do
  (_, i) <- get
  emit Jumpdest
  pure (Label i)

data Instr = Instr { instrAnnotation :: Maybe String, op :: Instr' }
  deriving (Show, Generic)

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
instance ToJSON Instr

push :: Integer -> Assembly; push = emit . Push
refer :: Label -> Assembly; refer = emit . PushLabel
dup :: Int -> Assembly; dup = emit . Dup
mstore :: Assembly; mstore = emit Mstore
mstore8 :: Assembly; mstore8 = emit Mstore8
mload :: Assembly; mload = emit Mload
msize :: Assembly; msize = emit Msize
pop :: Assembly; pop = emit Pop
eq :: Assembly; eq = emit Eq
stop :: Assembly; stop = emit Stop
swap :: Int -> Assembly; swap = emit . Swap
caller :: Assembly; caller = emit Caller
jumpi :: Assembly; jumpi = emit Jumpi
sload :: Assembly; sload = emit Sload
sstore :: Assembly; sstore = emit Sstore
byte :: Assembly; byte = emit Byte
calldatasize :: Assembly; calldatasize = emit Calldatasize
calldataload :: Assembly; calldataload = emit Calldataload
calldatacopy :: Assembly; calldatacopy = emit Calldatacopy
timestamp :: Assembly; timestamp = emit Timestamp
gaslimit :: Assembly; gaslimit = emit Gaslimit
gt :: Assembly; gt = emit Gt
lt :: Assembly; lt = emit Lt
add :: Assembly; add = emit Add
sub :: Assembly; sub = emit Sub
not :: Assembly; not = emit Not
call :: Assembly; call = emit Call
keccak256 :: Assembly; keccak256 = emit Keccak256
exp :: Assembly; exp = emit Exp
and :: Assembly; and = emit And
or :: Assembly; or = emit Or
div :: Assembly; div = emit Div
return :: Assembly; return = emit Return
iszero :: Assembly; iszero = emit Iszero
jump :: Assembly; jump = emit Jump
revert :: Assembly; revert = emit Revert
callvalue :: Assembly; callvalue = emit Callvalue
log :: Int -> Assembly; log = emit . Log

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
