let rfpath = ./dep/reflex-platform;
in { system ? builtins.currentSystem, obelisk ? import ./.obelisk/impl {
  reflex-platform-func = args@{ ... }:
    import rfpath (args // {
      inherit system;
      hlsSupport = true;
    });
  inherit system;
  iosSdkVersion = "13.2";
  config.android_sdk.accept_license = true;
  terms.security.acme.acceptTerms = true;
}, projectOverrides ? { }, withHoogle ? false }:
with obelisk;

project ./. ({ hackGet, pkgs, ... }@args:
  let beamSrc = hackGet dep/beam;
  in {
    inherit withHoogle;
    android.applicationId = "ca.srid.obelisk-rhyolite.template";
    android.displayName = "Obelisk+Rhyolite Example";
    ios.bundleIdentifier = "ca.srid.obelisk-rhyolite.template";
    ios.bundleName = "Obelisk+Rhyolite Example";

    packages = {
      beam-core = beamSrc + "/beam-core";
      beam-postgres = beamSrc + "/beam-postgres";
      beam-migrate = beamSrc + "/beam-migrate";
    };

    shellToolOverrides = ghc: super: {
      inherit (pkgs) nixfmt;
      inherit (pkgs.haskellPackages) cabal-plan cabal-fmt;
    };

    overrides = pkgs.lib.composeExtensions (import (hackGet ./dep/rhyolite) ({
      inherit (args) pkgs;
      inherit obelisk;
    })).haskellOverrides (self: super: {
      beam-postgres = pkgs.haskell.lib.dontCheck
        super.beam-postgres; # Requires PG to run tests
    });
  } // projectOverrides)
