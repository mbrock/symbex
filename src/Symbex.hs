{-# Language DeriveDataTypeable #-}
{-# Language RecursiveDo #-}
{-# Language NamedFieldPuns #-}

module Symbex where

import Prelude hiding (not)
import Control.Monad.RWS (RWS, MonadFix, tell, get, modify, runRWS)
import Data.Generics.Uniplate.Data
import Data.Data (Data)
import Data.Typeable (Typeable)

newtype Assembler i a =
  Assembler (RWS () [i] Int a)
  deriving (Functor, Applicative, Monad, MonadFix)

type Assembly = Assembler Instr ()

emit :: Instr -> Assembly
emit x = Assembler (tell [x] >> modify succ)

assemble :: Assembly -> [Instr]
assemble (Assembler x) = y
  where ((), _, y) = runRWS x () 0

label :: Assembler Instr Int
label = Assembler get

example :: [Instr]
example = assemble $ mdo
  caller
  dup 1; push 1; eq; push x; swap 1; jumpi
  dup 1; push 2; eq; push y; swap 1; jumpi
  dup 1; push 3; eq; push z; swap 1; jumpi
  pop; push 0; push 1; mstore; push 0; mload; stop
  x <- label; pop; push 10; stop
  y <- label; pop; push 11; stop
  z <- label; pop; push 12; stop

multisig :: [Instr]
multisig = assemble $ mdo
  push 1; push 10; caller; eq; push confirm; jumpi; pop
  push 2; push 11; caller; eq; push confirm; jumpi; pop
  push 3; push 12; caller; eq; push confirm; jumpi; pop
  push 4; push 13; caller; eq; push confirm; jumpi; pop
  push 5; push 14; caller; eq; push confirm; jumpi; pop
  push 6; push 15; caller; eq; push confirm; jumpi; pop
  nope <- label; stop

  confirm <- label
  push 32; calldatasize; gt; push trigger; jumpi
  push 0; calldataload; dup 1; sload
  dup 1; dup 4; byte; push nope; jumpi
  dup 1; push 0; mstore
  push 0; byte; push 1; add; push 0; mstore8
  push 1; swap 2; mstore8
  push 0; mload; swap 1; sstore; stop

  trigger <- label
  calldatasize; push 0; push 0; calldatacopy
  msize; push 0; keccak256
  dup 1; sload; push 0; byte; push 2; gt; push nope; jumpi
  push 0; mload; timestamp; gt; push nope; jumpi
  push 255; not; swap 1; sstore
  push 0; push 0; push 96; msize; sub; push 96
  push 64; mload; push 32; mload
  gaslimit; call

data Instr
  = Push Int
  | Dup Int
  | Pop
  | Swap Int
  | Caller
  | Eq
  | Jumpi
  | Stop
  | Mstore
  | Mstore8
  | Mload
  | Calldatasize
  | Gt
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
  deriving Show

data Value = Actual Int | Symbolic Variable
  deriving (Show, Eq, Data, Typeable)

data Variable
  = TheCaller
  | TheCalldatasize
  | TheCalldataWord Value
  | TheTimestamp
  | TheGaslimit
  | SomeCallResult
  | TheByte Value Value
  | SetByte Int Value Value
  | Size Memory
  | Equality Value Value
  | IsGreaterThan Value Value
  | Negation Value
  | Minus Value Value
  | Plus Value Value
  | TheHashOf Value Value Memory
  | MemoryAt Value Memory
  | StorageAt Value Memory
  | Max Value Value
  deriving (Show, Eq, Data, Typeable)

dependsOnCall :: Value -> Bool
dependsOnCall = elem (Symbolic SomeCallResult) . universe

type PC = Int
type Stack = [Value]
type Code = [Instr]

data Memory
  = Null
  | With Value Value Memory
  | WithByte Value Value Memory
  | WithCalldata (Value, Value, Value) Memory
  | WithCallResult (Value, Value) Memory
  | ArbitrarilyAltered Memory
  deriving (Show, Eq, Data, Typeable)

data State = State
  { stack   :: Stack
  , pc      :: PC
  , memory  :: Memory
  , storage :: Memory
  } deriving Show

data Possibility
  = Step State
  | Fork Value State State
  | StackUnderrun Instr
  | Done
  deriving Show

exec :: State -> Code -> Possibility
exec (State { pc }) c | pc >= length c =
  Done
exec (state @ State { stack, pc, memory, storage }) c =
  case c !! pc of
    Stop ->
      Done

    Push x ->
      Step $ state
        { stack = Actual x : stack
        , pc = succ pc }

    Jumpi ->
      case stack of
        (Actual j : x : stack') ->
          Fork x
            (state { stack = stack', pc = j })
            (state { stack = stack', pc = succ pc })
        (Symbolic _ : _) ->
          error "symbolic jump"
        _ ->
          StackUnderrun (c !! pc)

    Dup n ->
      if length stack >= n
      then Step $ state
        { stack = stack !! (n - 1) : stack
        , pc = succ pc }
      else StackUnderrun (c !! pc)

    Pop ->
      case stack of
        (_:stack') ->
          Step $ state
            { stack = stack'
            , pc = succ pc }
        _ ->
          StackUnderrun (c !! pc)

    Swap n ->
      if length stack >= n + 1
      then
        let stack' = (stack !! n) :
                       (drop 1 (take n stack) ++ drop (n - 1) stack)
        in Step $ state
          { stack = stack'
          , pc = succ pc }
      else
        StackUnderrun (c !! pc)

    Caller ->
      Step $ state
        { stack = Symbolic TheCaller : stack
        , pc = succ pc }

    Eq ->
      case stack of
        (x:y:stack') ->
          Step $ state
            { stack = Symbolic (Equality x y) : stack'
            , pc = succ pc }
        _ ->
          StackUnderrun (c !! pc)

    Mstore ->
      case stack of
        (x : y : stack') ->
          Step $ state
            { stack = stack'
            , pc = succ pc
            , memory = With x y memory }
        _ ->
          StackUnderrun (c !! pc)

    Mstore8 ->
      case stack of
        (x : y : stack') ->
          Step $ state
            { stack = stack'
            , pc = succ pc
            , memory = WithByte x y memory }
        _ ->
          StackUnderrun (c !! pc)

    Mload ->
      case stack of
        (x:stack') ->
          Step $ state
            { stack = Symbolic (MemoryAt x memory) : stack'
            , pc = succ pc }
        _ ->
          StackUnderrun (c !! pc)

    Sstore ->
      case stack of
        (x : y : stack') ->
          Step $ state
            { stack = stack'
            , pc = succ pc
            , storage = With x y storage }
        _ ->
          StackUnderrun (c !! pc)

    Sload ->
      case stack of
        (x:stack') ->
          Step $ state
            { stack = Symbolic (StorageAt x storage) : stack'
            , pc = succ pc }
        _ ->
          StackUnderrun (c !! pc)

    Calldatasize ->
      Step $ state
        { stack = Symbolic TheCalldatasize : stack
        , pc = succ pc }

    Gt ->
      case stack of
        (x:y:stack') ->
          Step $ state
            { stack = Symbolic (x `IsGreaterThan` y) : stack'
            , pc = succ pc }
        _ ->
          StackUnderrun (c !! pc)

    Calldataload ->
      case stack of
        (x:stack') ->
          Step $ state
            { stack = Symbolic (TheCalldataWord x) : stack'
            , pc = succ pc }
        _ -> StackUnderrun (c !! pc)

    Byte ->
      case stack of
        (i:x:stack') ->
          Step $ state
            { stack = Symbolic (TheByte i x) : stack'
            , pc = succ pc }
        _ -> StackUnderrun (c !! pc)

    Calldatacopy ->
      case stack of
        (xTo:xFrom:xSize:stack') ->
          Step $ state
            { stack = stack'
            , pc = succ pc
            , memory = WithCalldata (xSize, xFrom, xTo) memory
            }
        _ -> StackUnderrun (c !! pc)

    Msize ->
      Step $ state
        { stack = Symbolic (Size memory) : stack
        , pc = succ pc }

    Keccak256 ->
      case stack of
        (xOffset:xSize:stack') ->
          Step $ state
            { stack = Symbolic (TheHashOf xOffset xSize memory) : stack'
            , pc = succ pc }
        _ -> StackUnderrun (c !! pc)

    Timestamp ->
      Step $ state
        { stack = Symbolic TheTimestamp : stack
        , pc = succ pc }

    Gaslimit ->
      Step $ state
        { stack = Symbolic TheGaslimit : stack
        , pc = succ pc }

    Sub ->
      case stack of
        (x:y:stack') ->
          Step $ state
            { stack = Symbolic (Minus y x) : stack'
            , pc = succ pc }
        _ -> StackUnderrun (c !! pc)

    Add ->
      case stack of
        (x:y:stack') ->
          Step $ state
            { stack = Symbolic (Plus y x) : stack'
            , pc = succ pc }
        _ -> StackUnderrun (c !! pc)

    Call ->
      case stack of
        (_:_xTo:_xValue:_xInOffset:_xInSize:xOutOffset:xOutSize:stack') -> do
          Step $ (state
              { stack = Actual 1 : stack'
              , pc = succ pc
              , memory = WithCallResult (xOutOffset, xOutSize) memory
              , storage = ArbitrarilyAltered storage
              })
          -- Fork (Symbolic SomeCallResult)
          --   (state
          --     { stack = Actual 1 : stack'
          --     , pc = succ pc
          --     , memory = WithCallResult (xOutOffset, xOutSize) memory
          --     , storage = ArbitrarilyAltered storage
          --     })
          --   (state
          --     { stack = Actual 0 : stack'
          --     , pc = succ pc
          --     })
        _ -> StackUnderrun (c !! pc)
    Not ->
      case stack of
        (x:stack') ->
          Step $ state
            { stack = Symbolic (Negation x) : stack'
            , pc = succ pc }
        _ -> StackUnderrun (c !! pc)

-- !

data Outcome = Good | Bad String
  deriving Show

type Path = ([Value], State, Outcome)

step :: Code -> State -> [Path]
step c state =
  case exec state c of
    Done -> [([], state, Good)]
    StackUnderrun x -> [([], state, Bad (show x))]
    Step s' -> step c s'
    Fork p s1 s2 ->
      map (\(ps, s', o) -> (p : ps, s', o)) (step c s1)
        ++ map (\(ps, s', o) -> (Symbolic (Negation p) : ps, s', o)) (step c s2)

pathDoesCall :: Path -> Bool
pathDoesCall (x, _, _) = any dependsOnCall x

class Optimize a where
  optimize :: a -> Maybe a

memorySize :: Memory -> Value
memorySize = \case
  Null ->
    Actual 0
  WithCalldata (n, _, dst) m ->
    Symbolic (Max (memorySize m) (Symbolic (Plus dst n)))
  x ->
    Symbolic (Size x)

instance Optimize Value where
  optimize = \case
    Symbolic (Size x) -> Just (memorySize x)
    Symbolic (Max (Actual 0) x) -> Just x
    Symbolic (Plus (Actual 0) x) -> Just x
    Symbolic (MemoryAt x mem) -> resolveMemory x mem
    _ -> Nothing

resolveMemory :: Value -> Memory -> Maybe Value
resolveMemory _ Null = Just (Actual 0)
resolveMemory x (With y z m) =
  if x == y then Just z else resolveMemory x m
resolveMemory (Actual i) (WithByte (Actual j) z m) =
  if i == div j 32
  then
    -- We have information about one byte of the requested word.
    case resolveMemory (Actual i) m of
      Nothing -> Nothing
      Just x' -> Just (Symbolic (SetByte j z x'))
  else
    -- This byte is unrelated to the requested word.
    resolveMemory (Actual i) m
resolveMemory _ _ = Nothing

isArbitrarilyAltered :: Memory -> Bool
isArbitrarilyAltered m =
  case m of
    Null -> False
    With _ _ x -> isArbitrarilyAltered x
    WithByte _ _ x -> isArbitrarilyAltered x
    WithCalldata _ x -> isArbitrarilyAltered x
    WithCallResult _ x -> isArbitrarilyAltered x
    ArbitrarilyAltered _ -> True

-- Spam

--class Display a where
--  display :: a -> String
--
--instance Display Value where
--  display (Actual x) = show x
--  display (Symbolic v) = display v
--
--instance Display Int where
--  display = show
--
--instance Display Variable where
--  display TheCaller = "(caller)"
--  display TheCalldatasize = "(calldatasize)"
--  display TheTimestamp = "(timestamp)"
--  display TheGaslimit = "(gaslimit)"
--  display SomeCallResult = "(some-call-result)"
--  display (TheCalldataWord a) = "(calldataload " ++ display a ++ ")"
--  display (TheByte a b) = "(byte " ++ display a ++ " " ++ display b ++ ")"
--  display (SetByte a b c) = "(set-byte " ++ display a ++ " " ++ display b ++ " " ++ display c ++ ")"
--  display (Equality a b) = "(eq? " ++ display a ++ " " ++ display b ++ ")"
--  display (Minus a b) = "(- " ++ display a ++ " " ++ display b ++ ")"
--  display (Plus a b) = "(+ " ++ display a ++ " " ++ display b ++ ")"
--  display (a `IsGreaterThan` b) = "(> " ++ display a ++ " " ++ display b ++ ")"
--  display (Negation a) = "(not " ++ display a ++ ")"
--  display (Size a) = "(size " ++ display a ++ ")"
--  display (TheHashOf a b m) = "(keccak256 " ++ display a ++ " " ++ display b ++ " " ++ display m ++ ")"
--  display (MemoryAt x m) = "(mload " ++ display x ++ " " ++ display m ++ ")"
--  display (StorageAt x m) = "(sload " ++ display x ++ " " ++ display m ++ ")"
--  display (Max a b) = "(max " ++ display a ++ " " ++ display b ++ ")"
--
--instance Display Memory where
--  display Null = "(initial)"
--  display (With x y m) = "(set " ++ display x ++ " " ++ display y ++ " " ++ display m ++ ")"
--  display (WithByte x y m) = "(set-byte " ++ display x ++ " " ++ display y ++ " " ++ display m ++ ")"
--  display (WithCalldata (n, x, y) m) =
--    "(set-calldata " ++ display n ++ " " ++ display x ++ " " ++ display y ++ " " ++ display m ++ ")"
--  display (WithCallResult (x, y) m) =
--    "(set-call-result " ++ display x ++ " " ++ display y ++ " " ++ display m ++ ")"
--  display (ArbitrarilyAltered m) =
--    "(arbitrarily-altered " ++ display m ++ ")"

push :: Int -> Assembly; push = emit . Push
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
add :: Assembly; add = emit Add
sub :: Assembly; sub = emit Sub
not :: Assembly; not = emit Not
call :: Assembly; call = emit Call
keccak256 :: Assembly; keccak256 = emit Keccak256
