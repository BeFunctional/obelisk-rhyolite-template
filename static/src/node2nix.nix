(import (builtins.fetchTarball {
  name = "nixpkgs-unstable";
  url =
    "https://github.com/nixos/nixpkgs/archive/4989a246d7a390a859852baddb1013f825435cee.tar.gz";
  sha256 = "158iia656ds6i6pc672w54cnph4d44d0a218dkq6npzrbhd3vvbg";
}) { }).nodePackages.node2nix
