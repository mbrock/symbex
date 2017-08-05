{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language RecursiveDo #-}
{-# Language NamedFieldPuns #-}

module Symbex where

import Prelude hiding (not, and, or, exp, return, div, log)
import qualified Prelude

import Control.Monad.State (get, modify, execState)
import qualified Control.Monad.State as Monad
import Data.Generics.Uniplate.Data
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics
import Data.Aeson (ToJSON ())

type Assembler i a = Monad.State ([i],Integer) a
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
     pure r

label :: Assembler Instr Integer
label = do x <- fmap snd get; emit Jumpdest; pure x

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

weth :: [Instr]
weth = assemble $ mdo
  callvalue; iszero; push dispatch; jumpi -- Skip deposit if no value sent
  as "total supply slot" (push 1); not; as "total supply" sload; callvalue; add             -- Calculate new total supply
  push 1; not; as "total supply slot" sstore                          -- Save new total supply to storage
  caller; as "old target balance" sload; callvalue; add            -- Calculate new target balance
  caller; sstore                         -- Save new target balance to storage
  -- Emit `Deposit(address indexed, uint)'
  as "Deposit(address indexed, uint)" $ push 0xe1fffcc4923d04b559f4d29a8bfc6cda04eb5b0d3c460751c2402c5c5cc9109c
  callvalue; push 0; mstore; caller; swap 1; push 32; push 0; log 2

  dispatch <- as "dispatch" label
  push 0; calldataload; push 224; push 2; exp; as "signature" div          -- Determine function signature
  dup 1; as "transferFrom" $ push 0x23b872dd; eq; push transferFrom; jumpi
  dup 1; as "approve" $ push 0x095ea7b3; eq; push approve; jumpi
  dup 1; as "withdraw" $ push 0x2e1a7d4d; eq; push withdraw; jumpi
  dup 1; as "balanceOf" $ push 0x70a08231; eq; push balanceOf; jumpi
  dup 1; as "totalSupply" $ push 0x18160ddd; eq; push totalSupply; jumpi
  dup 1; as "allowance" $ push 0xdd62ed3e; eq; push allowance; jumpi
  dup 1; as "transfer" $ push 0xa9059cbb; eq; push transfer; jumpi
  exit <- as "exit" label
  stop
  fail <- as "fail" label
  revert

  balanceOf <- as "balanceOf" label
  push 4; as "address" calldataload; sload                  -- Load balance from storage
  push 0; mstore; push 32; push 0; return                  -- Return balance

  totalSupply <- as "totalSupply" label
  push 32; as "total supply slot" not; sload                          -- Load supply from storage
  push 0; mstore; push 32; push 0; return                  -- Return total supply

  allowance <- as "allowance" label
  push 36; as "spender" calldataload -- Load spender
  push 4; as "owner" calldataload        -- Load owner
  push 0; mstore; push 32; mstore                    -- Write addresses to memory
  push 64; push 0; keccak256; as "allowance" sload                  -- Load allowance from storage
  push 0; mstore; push 32; push 0; return                  -- Return allowance

  transfer <- as "transfer" label
  push 36; as "value to be transferred" calldataload
  push 4; as "recipient" calldataload
  caller
  push attemptTransfer; jump

  transferFrom <- as "transferFrom" label
  push 68; as "value to be transferred" calldataload
  push 36; as "to" calldataload
  push 4; as "from" calldataload

  attemptTransfer <- as "attemptTransfer" label
  push 160; push 2; exp; dup 3; dup 3; or; div; push exit; jumpi -- Abort if garbage in addresses
  dup 2; as "target balance" sload; dup 2; as "source balance" sload                 -- Load source and target balances
  dup 5; dup 2; lt; push exit; jumpi -- Abort if insufficient balance
  dup 3; caller; eq; push performTransfer; jumpi -- Skip ahead if source is caller
  dup 3; push 0; mstore; caller; push 32; mstore
  push 32; push 0; keccak256                        -- Determine allowance storage slot
  dup 1; as "allowance" sload                            -- Load allowance from storage
  push 1; as "infinite allowance symbol" not; dup 2; eq; push performTransfer; jumpi -- Skip ahead if allowance is max
  dup 7; dup 2; lt; push exit; jumpi -- Abort if allowance is too low
  dup 7; swap 2; sub; swap 2; sstore           -- Save new allowance to storage

  performTransfer <- as "performTransfer" label
  dup 5; swap 3; sub; dup 3; sstore            -- Save source balance to storage
  dup 4; swap 2; add; dup 3; sstore            -- Save target balance to storage
  -- Emit `Transfer(address indexed, address indexed, uint)'
  push 0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef
  swap 4; push 0; mstore; push 32; push 0; log 3
  push 1; push 0; mstore; push 32; push 0; return                -- Return true

  approve <- label
  push 36; calldataload; push 4; calldataload        -- Load spender and new allowance
  caller; push 0; mstore; dup 2; push 32; mstore
  push 64; push 0; keccak256                        -- Determine allowance storage slot
  dup 3; sstore                           -- Write new allowance to storage
  -- Emit `Approval(address indexed, address indexed, uint)'
  push 0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925
  swap 3; push 0; mstore; caller; swap 1; push 0; push 0; log 3
  push 1; push 0; mstore; push 32; push 0; return                -- Return true

  withdraw <- label
  push 4; calldataload                        -- Load amount to withdraw
  caller; sload                          -- Load source balance from storage
  dup 2; dup 2; sub                         -- Calculate new source balance
  dup 2; swap 1; gt; push exit; jumpi -- Abort if underflow occurred
  caller; sstore                         -- Save new source balance to storage
  push 1; not; dup 2; sload                      -- Load total supply from storage
  dup 2; swap 1; sub                        -- Decrement total supply
  push 1; not; sstore                          -- Save new total supply to storage
  push 0; push 0; push 0; push 0                               -- No return data and no calldata
  dup 5; caller                           -- Send withdrawal amount to caller
  gaslimit; call; iszero; push fail; jumpi -- Make call, aborting on failure
  -- Emit `Withdrawal(address indexed, uint)'
  push 0x7fcf532c15f0a6db0bd6d0e038bea71d30d808c7d98cb3bf7268a95bf5081b65
  swap 1; push 0; mstore; caller; swap 1; push 32; push 0; log 2
  push 1; push 0; mstore; push 32; push 0; return                -- Return true

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
  as "size of action hash" (push 32); calldatasize; gt; push trigger; jumpi
  push 0; as "action hash" calldataload; dup 1; as "old action state" sload
  push 2; dup 4; as "confirmation flag bitmask" exp
  dup 1; dup 3; as "user confirmation bit" and; push nope; jumpi
  dup 2; or
  push 255; not; as "new confirmation state" and
  swap 1; push 255; as "old confirmation count" and
  push 1; as "new confirmation count" add
  as "new action state" or; swap 1; sstore; stop

  trigger <- label
  calldatasize; push 0; push 0; as "full action" calldatacopy
  calldatasize; push 0; as "action hash" keccak256
  as "quorum" (push 2); dup 2; as "action state" sload;
  push 255; as "confirmation count" and; lt; push nope; jumpi
  push 0; as "deadline" calldataload; timestamp; gt; push nope; jumpi
  push 255; as "trigger state" not; swap 1; sstore
  push 0; push 0; push 96; calldatasize; sub; push 96
  push 64; calldataload; push 32; calldataload
  gaslimit; call; pop

data Instr = Instr { instrAnnotation :: Maybe String, op :: Instr' }
  deriving (Show, Generic)

data Instr'
  = Push Integer
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
  = Actual Integer
  | TheCaller
  | TheCalldatasize
  | TheCallvalue
  | TheCalldataWord AValue
  | TheTimestamp
  | TheGaslimit
  | SomeCallResult
  | TheByte AValue AValue
  | SetByte Integer AValue AValue
  | Size Memory
  | Equality AValue AValue
  | IsGreaterThan AValue AValue
  | IsLessThan AValue AValue
  | Negation AValue
  | Minus AValue AValue
  | Plus AValue AValue
  | DividedBy AValue AValue
  | TheHashOf AValue AValue Memory
  | MemoryAt AValue Memory
  | StorageAt AValue Memory
  | Max AValue AValue
  | Conjunction AValue AValue
  | Disjunction AValue AValue
  | Exponentiation AValue AValue
  | SetBit AValue AValue
  | IsBitSet AValue AValue
  | IsZero AValue
  deriving (Show, Eq, Data, Typeable, Generic)

data AValue = As
  { annotation :: Maybe String
  , value :: Value
  }
  deriving (Show, Eq, Data, Typeable, Generic)

dependsOnCall :: AValue -> Bool
dependsOnCall = elem SomeCallResult . universe . value

type PC = Integer
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
  | Reverted
  | Returned AValue AValue
  deriving (Show, Generic)

exec :: State -> Code -> Possibility
exec (State { pc }) c | fromInteger pc >= length c =
  Done
exec (state @ State { stack, pc, memory, storage }) c =
  let instr = c !! fromInteger pc
      a = instrAnnotation instr
  in case op instr of
    Stop ->
      Done

    Jumpdest ->
      Step $ state
        { pc = succ pc }

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
          StackUnderrun instr

    Jump ->
      case stack of
        (As _ (Actual j) : stack') ->
          Step $
            (state { stack = stack', pc = j })
        (_ : _) ->
          error "symbolic jump"
        _ ->
          StackUnderrun instr

    Dup n ->
      if length stack >= n
      then Step $ state
        { stack = stack !! (n - 1) : stack
        , pc = succ pc }
      else StackUnderrun instr

    Pop ->
      case stack of
        (_:stack') ->
          Step $ state
            { stack = stack'
            , pc = succ pc }
        _ ->
          StackUnderrun instr

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
        StackUnderrun instr

    Log n ->
      if length stack >= n + 2
      then
        Step $ state
          { stack = drop (n + 2) stack
          , pc = succ pc }
      else
        StackUnderrun instr

    Iszero ->
      case stack of
        (x:xs) ->
          Step $ state
            { stack = As a (IsZero x) : xs
            , pc = succ pc
            }
        _ ->
          StackUnderrun instr

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
          StackUnderrun instr

    Mstore ->
      case stack of
        (x : y : stack') ->
          Step $ state
            { stack = stack'
            , pc = succ pc
            , memory = With x y memory }
        _ ->
          StackUnderrun instr

    Mstore8 ->
      case stack of
        (x : y : stack') ->
          Step $ state
            { stack = stack'
            , pc = succ pc
            , memory = WithByte x y memory }
        _ ->
          StackUnderrun instr

    Mload ->
      case stack of
        (x:stack') ->
          Step $ state
            { stack = As a (MemoryAt x memory) : stack'
            , pc = succ pc }
        _ ->
          StackUnderrun instr

    Sstore ->
      case stack of
        (x : y : stack') ->
          Step $ state
            { stack = stack'
            , pc = succ pc
            , storage = With x y storage }
        _ ->
          StackUnderrun instr

    Sload ->
      case stack of
        (x:stack') ->
          Step $ state
            { stack = As a (StorageAt x storage) : stack'
            , pc = succ pc }
        _ ->
          StackUnderrun instr

    Calldatasize ->
      Step $ state
        { stack = As a TheCalldatasize : stack
        , pc = succ pc }

    Callvalue ->
      Step $ state
        { stack = As a TheCallvalue : stack
        , pc = succ pc }

    Gt ->
      case stack of
        (x:y:stack') ->
          Step $ state
            { stack = As a (x `IsGreaterThan` y) : stack'
            , pc = succ pc }
        _ ->
          StackUnderrun instr

    Div ->
      case stack of
        (x:y:stack') ->
          Step $ state
            { stack = As a (x `DividedBy` y) : stack'
            , pc = succ pc }
        _ ->
          StackUnderrun instr

    Revert ->
      Reverted

    Return ->
      case stack of
        (x:y:_) ->
          Returned x y
        _ ->
          StackUnderrun instr

    Lt ->
      case stack of
        (x:y:stack') ->
          Step $ state
            { stack = As a (x `IsLessThan` y) : stack'
            , pc = succ pc }
        _ ->
          StackUnderrun instr

    Calldataload ->
      case stack of
        (x:stack') ->
          Step $ state
            { stack = As a (TheCalldataWord x) : stack'
            , pc = succ pc }
        _ -> StackUnderrun instr

    Byte ->
      case stack of
        (i:x:stack') ->
          Step $ state
            { stack = As a (TheByte i x) : stack'
            , pc = succ pc }
        _ -> StackUnderrun instr

    Calldatacopy ->
      case stack of
        (xTo:xFrom:xSize:stack') ->
          Step $ state
            { stack = stack'
            , pc = succ pc
            , memory = WithCalldata (xSize, xFrom, xTo) memory
            }
        _ -> StackUnderrun instr

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
        _ -> StackUnderrun instr

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
        _ -> StackUnderrun instr

    Add ->
      case stack of
        (x:y:stack') ->
          Step $ state
            { stack = As a (Plus y x) : stack'
            , pc = succ pc }
        _ -> StackUnderrun instr

    Exp ->
      case stack of
        (x:y:stack') ->
          Step $ state
            { stack = As a (Exponentiation y x) : stack'
            , pc = succ pc }
        _ -> StackUnderrun instr

    And ->
      case stack of
        (x:y:stack') ->
          Step $ state
            { stack = As a (Conjunction y x) : stack'
            , pc = succ pc }
        _ -> StackUnderrun instr

    Or ->
      case stack of
        (x:y:stack') ->
          Step $ state
            { stack = As a (Disjunction y x) : stack'
            , pc = succ pc }
        _ -> StackUnderrun instr

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
        _ -> StackUnderrun instr
    Not ->
      case stack of
        (x:stack') ->
          Step $ state
            { stack = As a (Negation x) : stack'
            , pc = succ pc }
        _ -> StackUnderrun instr

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
    Reverted -> One state (Bad "reverted")
    Returned _ _ -> One state Good
    StackUnderrun x -> One state (Bad (show x))
    Step s' -> step' c s'
    Fork p s1 s2 ->
      Two p (step' c s1) (step' c s2)

step :: Code -> State -> [Path]
step c state =
  case exec state c of
    Done -> [Path [] state Good]
    StackUnderrun x -> [Path [] state (Bad (show x))]
    Reverted -> [Path [] state (Bad "reverted")]
    Returned _ _ -> [Path [] state Good]
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
  if i == Prelude.div j 32
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

push :: Integer -> Assembly; push = emit . Push
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
