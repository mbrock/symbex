{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language RecursiveDo #-}
{-# Language NamedFieldPuns #-}

module Symbex where

import Prelude hiding (not, and, or, exp)
import Control.Monad.State (get, modify, execState)
import qualified Control.Monad.State as Monad
import Data.Generics.Uniplate.Data
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics
import Data.Aeson (ToJSON ())

type Assembler i a = Monad.State ([i],Int) a
type Assembly = Assembler Instr ()

emit :: Instr' -> Assembly
emit x = modify (\(xs, i) -> (Instr Nothing x : xs, succ i))

assemble :: Assembly -> [Instr]
assemble x = reverse (fst y)
  where y = execState x ([], 0)

as :: String -> Assembler Instr a -> Assembler Instr a
as s m =
  do r <- m
     modify $ \(Instr _ x : xs, i) -> (Instr (Just s) x : xs, i)
     return r

label :: Assembler Instr Int
label = fmap snd get

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

multisig2 :: [Instr]
multisig2 = assemble $ mdo

  let
    allow s i j = do
      as ("id of " ++ s)      (push i)
      as ("address of " ++ s) (push j)
      caller; eq; push confirm; jumpi; pop

  allow "bob" 8 10
  allow "pam" 9 11
  allow "tom" 10 12
  allow "ken" 11 13
  allow "liz" 12 14
  allow "joe" 13 15
  nope <- label; stop

  confirm <- label
  as "size of confirmation" (push 32); calldatasize; gt; push trigger; jumpi
  push 0; as "action hash" calldataload; dup 1; as "old action state" sload
  push 2; dup 4; as "confirmation flag bitmask" exp
  dup 1; dup 3; as "confirmation flag" and; push nope; jumpi
  dup 2; or
  push 255; not; as "new confirmations" and
  swap 1; push 255; as "old confirmation count" and
  push 1; as "new confirmation count" add
  as "new action state" or; swap 1; sstore; stop

  trigger <- label
  calldatasize; push 0; push 0; as "full action" calldatacopy
  calldatasize; push 0; as "action hash" keccak256
  as "quorum" (push 2); dup 2; as "action state" sload;
  push 255; as "confirmation count" and; lt; push nope; jumpi
  push 0; as "deadline" calldataload; timestamp; gt; push nope; jumpi
  push 255; as "triggered" not; swap 1; sstore
  push 0; push 0; push 96; calldatasize; sub; push 96
  push 64; calldataload; push 32; calldataload
  gaslimit; call; pop

data Instr = Instr { instrAnnotation :: Maybe String, op :: Instr' }
  deriving (Show, Generic)

data Instr'
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
  deriving (Show, Generic)

instance ToJSON Instr'
instance ToJSON Instr
instance ToJSON Value
instance ToJSON AValue
instance ToJSON Memory
instance ToJSON State
instance ToJSON Possibility
instance ToJSON Path
instance ToJSON Outcome
instance ToJSON Tree

data Value
  = Actual Int
  | TheCaller
  | TheCalldatasize
  | TheCalldataWord AValue
  | TheTimestamp
  | TheGaslimit
  | SomeCallResult
  | TheByte AValue AValue
  | SetByte Int AValue AValue
  | Size Memory
  | Equality AValue AValue
  | IsGreaterThan AValue AValue
  | IsLessThan AValue AValue
  | Negation AValue
  | Minus AValue AValue
  | Plus AValue AValue
  | TheHashOf AValue AValue Memory
  | MemoryAt AValue Memory
  | StorageAt AValue Memory
  | Max AValue AValue
  | Conjunction AValue AValue
  | Disjunction AValue AValue
  | Exponentiation AValue AValue
  | SetBit AValue AValue
  | IsBitSet AValue AValue
  deriving (Show, Eq, Data, Typeable, Generic)

data AValue = As
  { annotation :: Maybe String
  , value :: Value
  }
  deriving (Show, Eq, Data, Typeable, Generic)

dependsOnCall :: AValue -> Bool
dependsOnCall = elem SomeCallResult . universe . value

type PC = Int
type Stack = [AValue]
type Code = [Instr]

data Memory
  = Null
  | With AValue AValue Memory
  | WithByte AValue AValue Memory
  | WithCalldata (AValue, AValue, AValue) Memory
  | WithCallResult (AValue, AValue) Memory
  | ArbitrarilyAltered Memory
  deriving (Show, Eq, Data, Typeable, Generic)

data State = State
  { stack   :: Stack
  , pc      :: PC
  , memory  :: Memory
  , storage :: Memory
  } deriving (Show, Generic)

data Possibility
  = Step State
  | Fork AValue State State
  | StackUnderrun Instr
  | Done
  deriving (Show, Generic)

exec :: State -> Code -> Possibility
exec (State { pc }) c | pc >= length c =
  Done
exec (state @ State { stack, pc, memory, storage }) c =
  let a = instrAnnotation (c !! pc)
  in case op (c !! pc) of
    Stop ->
      Done

    Push x ->
      Step $ state
        { stack = As a (Actual x) : stack
        , pc = succ pc }

    Jumpi ->
      case stack of
        (As _ (Actual j) : x : stack') ->
          Fork x
            (state { stack = stack', pc = j })
            (state { stack = stack', pc = succ pc })
        (_ : _ : _) ->
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
      -- swap 5
      -- 1 2 3 4 5 6 7 8 9 10
      -- 6 : 1 2 3 4 5 : 7 8 9 10
      -- #n : take n : drop (n + 1)
      if length stack >= n + 1
      then
        let stack' = (stack !! n) :
                       (take n stack ++ drop (n + 1) stack)
        in Step $ state
          { stack = stack'
          , pc = succ pc }
      else
        StackUnderrun (c !! pc)

    Caller ->
      Step $ state
        { stack = As a TheCaller : stack
        , pc = succ pc }

    Eq ->
      case stack of
        (x:y:stack') ->
          Step $ state
            { stack = As a (Equality x y) : stack'
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
            { stack = As a (MemoryAt x memory) : stack'
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
            { stack = As a (StorageAt x storage) : stack'
            , pc = succ pc }
        _ ->
          StackUnderrun (c !! pc)

    Calldatasize ->
      Step $ state
        { stack = As a TheCalldatasize : stack
        , pc = succ pc }

    Gt ->
      case stack of
        (x:y:stack') ->
          Step $ state
            { stack = As a (x `IsGreaterThan` y) : stack'
            , pc = succ pc }
        _ ->
          StackUnderrun (c !! pc)

    Lt ->
      case stack of
        (x:y:stack') ->
          Step $ state
            { stack = As a (x `IsLessThan` y) : stack'
            , pc = succ pc }
        _ ->
          StackUnderrun (c !! pc)

    Calldataload ->
      case stack of
        (x:stack') ->
          Step $ state
            { stack = As a (TheCalldataWord x) : stack'
            , pc = succ pc }
        _ -> StackUnderrun (c !! pc)

    Byte ->
      case stack of
        (i:x:stack') ->
          Step $ state
            { stack = As a (TheByte i x) : stack'
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
        { stack = As a (Size memory) : stack
        , pc = succ pc }

    Keccak256 ->
      case stack of
        (xOffset:xSize:stack') ->
          Step $ state
            { stack = As a (TheHashOf xOffset xSize memory) : stack'
            , pc = succ pc }
        _ -> StackUnderrun (c !! pc)

    Timestamp ->
      Step $ state
        { stack = As a TheTimestamp : stack
        , pc = succ pc }

    Gaslimit ->
      Step $ state
        { stack = As a TheGaslimit : stack
        , pc = succ pc }

    Sub ->
      case stack of
        (x:y:stack') ->
          Step $ state
            { stack = As a (Minus y x) : stack'
            , pc = succ pc }
        _ -> StackUnderrun (c !! pc)

    Add ->
      case stack of
        (x:y:stack') ->
          Step $ state
            { stack = As a (Plus y x) : stack'
            , pc = succ pc }
        _ -> StackUnderrun (c !! pc)

    Exp ->
      case stack of
        (x:y:stack') ->
          Step $ state
            { stack = As a (Exponentiation y x) : stack'
            , pc = succ pc }
        _ -> StackUnderrun (c !! pc)

    And ->
      case stack of
        (x:y:stack') ->
          Step $ state
            { stack = As a (Conjunction y x) : stack'
            , pc = succ pc }
        _ -> StackUnderrun (c !! pc)

    Or ->
      case stack of
        (x:y:stack') ->
          Step $ state
            { stack = As a (Disjunction y x) : stack'
            , pc = succ pc }
        _ -> StackUnderrun (c !! pc)

    Call ->
      case stack of
        (_:_xTo:_xValue:_xInOffset:_xInSize:xOutOffset:xOutSize:stack') -> do
          Step $ (state
              { stack = As a (Actual 1) : stack'
              , pc = succ pc
              , memory = WithCallResult (xOutOffset, xOutSize) memory
              , storage = ArbitrarilyAltered storage
              })
          -- Fork (SomeCallResult)
          --   (state
          --     { stack = As a (Actual 1) : stack'
          --     , pc = succ pc
          --     , memory = WithCallResult (xOutOffset, xOutSize) memory
          --     , storage = ArbitrarilyAltered storage
          --     })
          --   (state
          --     { stack = As a (Actual 0) : stack'
          --     , pc = succ pc
          --     })
        _ -> StackUnderrun (c !! pc)
    Not ->
      case stack of
        (x:stack') ->
          Step $ state
            { stack = As a (Negation x) : stack'
            , pc = succ pc }
        _ -> StackUnderrun (c !! pc)

-- !

data Outcome = Good | Bad String
  deriving (Show, Generic)

data Path = Path [AValue] State Outcome
  deriving Generic

data Tree = One State Outcome | Two AValue Tree Tree
  deriving Generic

step' :: Code -> State -> Tree
step' c state =
  case exec state c of
    Done -> One state Good
    StackUnderrun x -> One state (Bad (show x))
    Step s' -> step' c s'
    Fork p s1 s2 ->
      Two p (step' c s1) (step' c s2)

step :: Code -> State -> [Path]
step c state =
  case exec state c of
    Done -> [Path [] state Good]
    StackUnderrun x -> [Path [] state (Bad (show x))]
    Step s' -> step c s'
    Fork p s1 s2 ->
      map (\(Path ps s' o) -> Path (p : ps) s' o) (step c s1)
        ++ map (\(Path ps s' o) -> Path (As Nothing (Negation p) : ps) s' o) (step c s2)

pathDoesCall :: Path -> Bool
pathDoesCall (Path x _ _) = any dependsOnCall x

class Optimize a where
  optimize :: a -> Maybe a

memorySize :: Memory -> Value
memorySize = \case
  Null ->
    Actual 0
  WithCalldata (n, _, dst) m ->
    Max (As Nothing (memorySize m)) (As Nothing (Plus dst n))
  x ->
    Size x

instance Optimize AValue where
  optimize (As a instr) = case instr of
    Size x -> Just (As a (memorySize x))
    Max (As _ (Actual 0)) x -> Just x
    Plus (As _ (Actual 0)) x -> Just x
    MemoryAt x mem -> resolveMemory a x mem
    Disjunction (As _ (Exponentiation (As _ (Actual 2)) (As _ (Actual x)))) y ->
      Just (As a (SetBit (As Nothing (Actual (x + 1))) y))
    Conjunction (As _ (Exponentiation (As _ (Actual 2)) (As _ (Actual x)))) y ->
      Just (As a (IsBitSet (As Nothing (Actual (x + 1))) y))
    _ -> Nothing

resolveMemory :: Maybe String -> AValue -> Memory -> Maybe AValue
resolveMemory a _ Null = Just (As a (Actual 0))
resolveMemory a x (With y z m) =
  if value x == value y then Just z else resolveMemory a x m
resolveMemory a (As _ (Actual i)) (WithByte (As _ (Actual j)) z m) =
  if i == div j 32
  then
    -- We have information about one byte of the requested word.
    case resolveMemory a (As Nothing (Actual i)) m of
      Nothing -> Nothing
      Just (As a' x') -> Just (As Nothing (SetByte j z (As a' x')))
  else
    -- This byte is unrelated to the requested word.
    resolveMemory a (As Nothing (Actual i)) m
resolveMemory _ _ _ = Nothing

isArbitrarilyAltered :: Memory -> Bool
isArbitrarilyAltered m =
  case m of
    Null -> False
    With _ _ x -> isArbitrarilyAltered x
    WithByte _ _ x -> isArbitrarilyAltered x
    WithCalldata _ x -> isArbitrarilyAltered x
    WithCallResult _ x -> isArbitrarilyAltered x
    ArbitrarilyAltered _ -> True

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
lt :: Assembly; lt = emit Lt
add :: Assembly; add = emit Add
sub :: Assembly; sub = emit Sub
not :: Assembly; not = emit Not
call :: Assembly; call = emit Call
keccak256 :: Assembly; keccak256 = emit Keccak256
exp :: Assembly; exp = emit Exp
and :: Assembly; and = emit And
or :: Assembly; or = emit Or
