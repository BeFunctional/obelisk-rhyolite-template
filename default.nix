let rfpath = ./dep/reflex-platform;
in { system ? builtins.currentSystem, obelisk ? import ./.obelisk/impl {
  reflex-platform-func = args@{ ... }:
    import rfpath (args // { inherit system; });
  inherit system;
  iosSdkVersion = "13.2";
  config.android_sdk.accept_license = true;
  terms.security.acme.acceptTerms = true;
}, projectOverrides ? { }, withHoogle ? false }:
with obelisk;

project ./. ({ hackGet, pkgs, ... }@args:
  let
    hls = obelisk.nixpkgs.haskell-language-server.override {
      inherit (obelisk.nixpkgs) haskell haskellPackages;
      supportedGhcVersions = [ "8107" ];
    };
  in {
    inherit withHoogle;
    android.applicationId = "ca.srid.obelisk-rhyolite.template";
    android.displayName = "Obelisk+Rhyolite Example";
    ios.bundleIdentifier = "ca.srid.obelisk-rhyolite.template";
    ios.bundleName = "Obelisk+Rhyolite Example";

    packages = { };

    shellToolOverrides = ghc: super: {
      inherit (pkgs) nixfmt;
      inherit (pkgs.haskellPackages)
        cabal-plan cabal-fmt;
        haskell-language-server = hls;
    };

    overrides = pkgs.lib.composeExtensions (import (hackGet ./dep/rhyolite) ({
      inherit (args) pkgs;
      inherit obelisk;
      inherit (args.pkgs.darwin.apple_sdk) sdk;
      inherit (args.pkgs.darwin.apple_sdk.frameworks) Cocoa;
    })).haskellOverrides (self: super: {
      beam-postgres = pkgs.haskell.lib.dontCheck
        super.beam-postgres; # Requires PG to run tests
    });
  } // projectOverrides)
