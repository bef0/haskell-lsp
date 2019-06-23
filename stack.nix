{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
    inherit (pkgs.ghc);
    name = "haskell-lsp";
    src = ./.;
    buildInputs = [ clang ];
}