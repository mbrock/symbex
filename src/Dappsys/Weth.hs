{-# Language RecursiveDo #-}

module Dappsys.Weth where

import Prelude (($))
import Symbex

weth :: [Instr]
weth = assemble $ mdo

  let
    addsafe = do
      dup 1; swap 2; add; dup 1; swap 2
      lt ? "overflow"; push fail; jumpi
  
  -- Skip deposit if no value sent
  callvalue; iszero; push dispatch; jumpi

  -- Calculate new total supply
  push 1; not ? "total supply slot"; sload ? "total supply"; callvalue; addsafe

  -- Save new total supply to storage
  push 1; not ? "total supply slot"; sstore

  -- Calculate new target balance
  caller; sload ? "old target balance"; callvalue; addsafe

  -- Save new target balance to storage
  caller; sstore

  -- Emit `Deposit(address indexed, uint)'
  push 0xe1fffcc4923d04b559f4d29a8bfc6cda04eb5b0d3c460751c2402c5c5cc9109c
    ? "Deposit(address indexed, uint)"
  callvalue; push 0; mstore; caller; swap 1; push 32; push 0; log 2

  -- Determine function signature
  dispatch <- label ? "dispatch"
  push 0; calldataload; push 224; push 2; exp; div ? "signature"

  dup 1; push 0x23b872dd ? "transferFrom"; eq; push transferFrom; jumpi
  dup 1; push 0x095ea7b3 ? "approve"; eq; push approve; jumpi
  dup 1; push 0x2e1a7d4d ? "withdraw"; eq; push withdraw; jumpi
  dup 1; push 0x70a08231 ? "balanceOf"; eq; push balanceOf; jumpi
  dup 1; push 0x18160ddd ? "totalSupply"; eq; push totalSupply; jumpi
  dup 1; push 0xdd62ed3e ? "allowance"; eq; push allowance; jumpi
  dup 1; push 0xa9059cbb ? "transfer"; eq; push transfer; jumpi

  fail <- label ? "fail"
  revert

  balanceOf <- label ? "balanceOf"

  -- Load balance from storage
  push 4; calldataload ? "address"; sload

  -- Return balance
  push 0; mstore; push 32; push 0; return

  totalSupply <- label ? "totalSupply"

  -- Load supply from storage
  push 32; not ? "total supply slot"; sload

  -- Return total supply
  push 0; mstore; push 32; push 0; return

  allowance <- label ? "allowance"

  -- Load spender
  push 36; calldataload ? "spender"

  -- Load owner
  push 4; calldataload ? "owner"

  -- Write addresses to memory
  push 0; mstore; push 32; mstore

  -- Load allowance from storage
  push 64; push 0; keccak256; sload ? "allowance"

  -- Return allowance
  push 0; mstore; push 32; push 0; return

  transfer <- label ? "transfer"
  push 36; calldataload ? "value to be transferred"
  push 4;  calldataload ? "recipient"
  caller; push attemptTransfer; jump

  transferFrom <- label ? "transferFrom"
  push 68 ; calldataload ? "value to be transferred"
  push 36 ; calldataload ? "to"
  push 4  ; calldataload ? "from"

  attemptTransfer <- label ? "attemptTransfer"

  -- Abort if garbage in addresses
  push 160; push 2; exp; dup 3; dup 3; or; div; push fail; jumpi

  -- Load source and target balances
  dup 2; sload ? "target balance"; dup 2; sload ? "source balance"

  -- Abort if insufficient balance
  dup 5; dup 2; lt; push fail; jumpi

  -- Skip ahead if source is caller
  dup 3; caller; eq; push performTransfer; jumpi
  dup 3; push 0; mstore; caller; push 32; mstore

  -- Determine allowance storage slot
  push 32; push 0; keccak256

  -- Load allowance from storage
  dup 1; sload ? "allowance"

  -- Skip ahead if allowance is max
  push 1; not ? "infinite allowance symbol"; dup 2
  eq; push performTransfer; jumpi

  -- Abort if allowance is too low
  dup 7; dup 2; lt; push fail; jumpi

  -- Save new allowance to storage
  dup 7; swap 2; sub; swap 2; sstore

  performTransfer <- label ? "performTransfer"

  -- Save source balance to storage
  dup 5; swap 3; sub; dup 3; sstore

  -- Save target balance to storage
  dup 4; swap 2; addsafe; dup 3; sstore

  -- Emit `Transfer(address indexed, address indexed, uint)'
  push 0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef
    ? "Transfer(address indexed, address indexed, uint)"
  swap 4; push 0; mstore; push 32; push 0; log 3

  -- Return true
  push 1; push 0; mstore; push 32; push 0; return

  approve <- label ? "approve"

  -- Load spender and new allowance
  push 36; calldataload; push 4; calldataload
  caller; push 0; mstore; dup 2; push 32; mstore

  -- Determine allowance storage slot
  push 64; push 0; keccak256

  -- Write new allowance to storage
  dup 3; sstore

  -- Emit `Approval(address indexed, address indexed, uint)'
  push 0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925
  swap 3; push 0; mstore; caller; swap 1; push 0; push 0; log 3

  -- Return true
  push 1; push 0; mstore; push 32; push 0; return

  withdraw <- label

  -- Load amount to withdraw
  push 4; calldataload

  -- Load source balance from storage
  caller; sload

  -- Calculate new source balance
  dup 2; dup 2; sub

  -- Abort if underflow occurred
  dup 2; swap 1; gt; push fail; jumpi

  -- Save new source balance to storage
  caller; sstore

  -- Load total supply from storage
  push 1; not; dup 2; sload

  -- Decrement total supply
  dup 2; swap 1; sub

  -- Save new total supply to storage
  push 1; not; sstore

  -- No return data and no calldata
  push 0; push 0; push 0; push 0

  -- Send withdrawal amount to caller
  dup 5; caller

  -- Make call, aborting on failure
  gaslimit; call; iszero; push fail; jumpi

  -- Emit `Withdrawal(address indexed, uint)'
  push 0x7fcf532c15f0a6db0bd6d0e038bea71d30d808c7d98cb3bf7268a95bf5081b65
  swap 1; push 0; mstore; caller; swap 1; push 32; push 0; log 2

  -- Return true
  push 1; push 0; mstore; push 32; push 0; return
