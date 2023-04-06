let rfpath = ./dep/reflex-platform;
in { system ? builtins.currentSystem, obelisk ? import ./.obelisk/impl {
  reflex-platform-func = args@{ ... }:
    import rfpath (args // {
      inherit system;
      __useNewerCompiler = true;
    });
  inherit system;
  iosSdkVersion = "13.2";
  config.android_sdk.accept_license = true;
  terms.security.acme.acceptTerms = true;
}, projectOverrides ? { }, withHoogle ? false }:
with obelisk;

project ./. ({ hackGet, pkgs, ... }@args:
  let
      inherit (pkgs.haskell.packages.ghc8107) haskell-language-server;
  in {
    inherit withHoogle;
    android.applicationId = "ca.srid.obelisk-rhyolite.template";
    android.displayName = "Obelisk+Rhyolite Example";
    ios.bundleIdentifier = "ca.srid.obelisk-rhyolite.template";
    ios.bundleName = "Obelisk+Rhyolite Example";

    shellToolOverrides = ghc: super: {
      inherit haskell-language-server;
      inherit (pkgs) nixfmt;
      inherit (pkgs.haskellPackages) cabal-plan cabal-fmt;
    };

    overrides = (pkgs.lib.composeExtensions (import (hackGet ./dep/rhyolite) ({
      inherit (args) pkgs;
      inherit obelisk;
    })).haskellOverrides (self: super:
      with pkgs.haskell.lib;
      let
        isGHCJS = self.ghc.isGhcjs or false;
        dontCheckGHCJS = if isGHCJS then dontCheck else x: x;
        dontHaddockGHCJS = if isGHCJS then dontHaddock else x: x;
      in let
        githubRepo = fq: rev:
          builtins.fetchTarball
          ("https://github.com/" + fq + "/archive/" + rev + ".tar.gz");
        callHackageDirectRevisionOverride = attrbs:
          overrideCabal (self.callHackageDirect
            (removeAttrs attrbs [ "editedCabalFile" "revision" ]) { })
          (old: { inherit (attrbs) editedCabalFile revision; });
        srcs = {
          universe = githubRepo "dmwit/universe"
            "f911a258b89074a0ff5edc583d62ff3bd17b9529";
          dependent-sum = githubRepo "obsidiansystems/dependent-sum"
            "43c633312b1d706a81a01c61cc3a33bdbe5530a3";
          dependent-sum-aeson-orphans =
            githubRepo "obsidiansystems/dependent-sum-aeson-orphans"
            "2c09bed287f57d23b1c04542b92325da1144d0a1";
          aeson-gadt-th = githubRepo "obsidiansystems/aeson-gadt-th"
            "71a315a4873c2875ad737a3320320849bdcf8a2a";
          dependent-map = githubRepo "obsidiansystems/dependent-map"
            "8e62fa50125ed2388ae3e2a7eee6dbfe25a02af0";
          optics = githubRepo "well-typed/optics"
            "3c6beec69ef5d33f18d1d58729eba2bb400be77c";
        };
      in {
        beam-core = dontCheck super.beam-core;
        vessel = dontCheck (self.callHackageDirect {
          pkg = "vessel";
          ver = "0.3.0.0";
          sha256 = "oTIfz2ZQL9t9SXW3Wdag0VJ/gzkIqixk3dNg2ECbx5c=";
        } { });
        universe = dontCheck
          (self.callCabal2nixWithOptions "universe" srcs.universe
            "--subpath universe" { });
        universe-some = callHackageDirectRevisionOverride {
          pkg = "universe-some";
          ver = "1.2.1";
          sha256 = "hqzL+VdYTKKuFxtFeHGcstR+P3W4DPAbVU2bYWlzK9o=";
          editedCabalFile =
            "73d7082e6516d9c22f7d069750e4a4be7e7e5068e7e1bdc0461ce1c955305ed9";
          revision = "4";
        };
        universe-dependent-sum = dontCheck
          (self.callCabal2nixWithOptions "universe-dependent-sum" srcs.universe
            "--subpath universe-dependent-sum" { });
        # #  We don't use the above for these because the license file isn't where nix needs it. :()
        OneTuple = self.callHackage "OneTuple" "0.3.1" { };
        hashable = self.callHackage "hashable" "1.3.5.0" { };
        universe-base = self.callHackageDirect {
          pkg = "universe-base";
          ver = "1.1.3";
          sha256 = "/bn9V2IuouHiaqte1yGXkWslB+eSXhH6uJNeziVlQeI=";
        } { };
        universe-instances-extended = self.callHackageDirect {
          pkg = "universe-instances-extended";
          ver = "1.1.3";
          sha256 = "wmx1IEvGmbudemPPtGSmITB8Tq9oHKIjt1Fb3zSsadE=";
        } { };
        universe-reverse-instances = callHackageDirectRevisionOverride {
          pkg = "universe-reverse-instances";
          ver = "1.1.1";
          sha256 = "1nEqrVpuCRCuo4tucVUR+iWNUVlN8xrK4ssUgw8Todk=";
          editedCabalFile =
            "bda5c74a274d06bb7d7d0682f7a2c0755d4b4285e6cdd20d632d9f318410e9d6";
          revision = "2";
        };

        beam-postgres =
          dontCheck super.beam-postgres; # Requires PG to run tests

        some = self.callHackageDirect {
          pkg = "some";
          ver = "1.0.4.1";
          sha256 = "4Ao6asEguIRfpZBgC4yW89wWg+zVPvBG3DA8VPBTlvM=";
        } { };

        constraints-extras = self.callCabal2nix "constraints-extras"
          (pkgs.fetchFromGitHub {
            owner = "obsidiansystems";
            repo = "constraints-extras";
            rev = "dbdd453f825fa81281bb871b5666257c4654d156";
            sha256 = "n2DmbVfR6o8t2G4C3MAtExO7h9mXRFCRy+ZeKsQBwms=";
          }) { };
        dependent-sum = dontCheck (self.callCabal2nix "dependent-sum"
          (srcs.dependent-sum + "/dependent-sum") { });
        dependent-sum-template = dontCheck
          (self.callCabal2nix "dependent-sum-template"
            (srcs.dependent-sum + "/dependent-sum-template") { });
        dependent-sum-aeson-orphans = dontCheck
          (self.callCabal2nix "dependent-sum-aeson-orphans"
            (srcs.dependent-sum-aeson-orphans) { });
        aeson-gadt-th = dontCheck
          (self.callCabal2nix "aeson-gadt-th" (srcs.aeson-gadt-th) { });
        dependent-map =
          dontCheck (self.callCabal2nix "dependent-map" srcs.dependent-map { });


      }));
  } // projectOverrides)
