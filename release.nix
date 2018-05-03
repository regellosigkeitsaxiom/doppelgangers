let
  pkgs = import <nixpkgs> {};
  source = import ./default.nix {};

  jobs = rec {
    build = source;
  };
in
  jobs
