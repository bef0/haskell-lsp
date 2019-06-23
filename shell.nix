let pkgs = import <nixpkgs> { };
in  import ./stack.nix { ghc = pkgs.ghc; }