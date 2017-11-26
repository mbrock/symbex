weth.org: nix; cabal run --verbose=0 -- weth > weth.org
nix: default.nix symbex.nix; nix-shell -A symbex.env symbex.nix \
  --command "cabal configure"
default.nix: symbex.cabal; cabal2nix . > default.nix
