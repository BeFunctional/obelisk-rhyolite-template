{ system ? builtins.currentSystem,
  obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "15.0";
    config.allowBroken = true;
    config.android_sdk.accept_license = true;
    terms.security.acme.acceptTerms = true;
    reflex-platform-func = import ./dep/reflex-platform;
    useGHC810 = true;
  },
  projectOverrides ? { },
  withHoogle ? false
}:

with obelisk;

project ./. ({ hackGet, pkgs, ... }@args:
  let
    inherit (pkgs) callPackage fetchFromGitHub mkShell;
  in {
    inherit withHoogle;
    android.applicationId = "obelisk.rhyolite.template";
    android.displayName = "obelisk rhyolite template";
    ios.bundleIdentifier = "obelisk.rhyolite.template";
    ios.bundleName = "obelisk rhyolite template";

    packages = {};

    shellToolOverrides = ghc: super: {
      inherit (pkgs) zlib nixfmt vips mupdf;
      inherit (pkgs.haskellPackages) cabal-plan cabal-fmt ;
      inherit (pkgs.haskell.packages.ghc8107) haskell-language-server;
    };


    overrides = pkgs.lib.composeExtensions
      ( import (hackGet ./dep/rhyolite) ({
          inherit pkgs;
          inherit obelisk;
        })
      ).haskellOverrides
      ( self: super:
          with pkgs.haskell.lib;
          let
            isGHCJS = self.ghc.isGhcjs or false;
            dontCheckGHCJS = if isGHCJS then dontCheck else x: x;
            dontHaddockGHCJS = if isGHCJS then dontHaddock else x: x;
            githubRepo = fq: rev:
              builtins.fetchTarball
              ("https://github.com/" + fq + "/archive/" + rev + ".tar.gz");
            callHackageDirectRevisionOverride = attrbs:
              overrideCabal
                (self.callHackageDirect
                  (removeAttrs attrbs [ "editedCabalFile" "revision" ]) { })
                (old: { inherit (attrbs) editedCabalFile revision; });
            srcs = {
              generically = githubRepo "haskell-compat/generically"
                "3b18e23dc2eaba9235c434f1fbe9461987a369b4";
            };
          in {
            # disable tests that cause errors
            aeson-casing = dontCheck super.aeson-casing;
            amazonka = dontCheck super.amazonka;
            amazonka-cognito-idp = dontCheck super.amazonka-cognito-idp;
            beam-core = dontCheck super.beam-core;
            beam-postgres = dontCheck
              super.beam-postgres; # requires PG to run tests
            generic-data = dontCheck super.generic-data;
            hls-brittany-plugin = dontCheck super.hls-brittany-plugin;
            http-media = dontCheck super.http-media;
            hspec-golden-aeson = dontCheck super.hspec-golden-aeson;
            ip = dontCheck super.ip; # marked as broken
            keccak = dontCheck (unmarkBroken super.keccak);
            relapse = dontCheck (unmarkBroken super.relapse);
            swagger2 = dontCheck super.swagger2;
            vinyl = dontCheck super.vinyl;

            # pin package versions
            OneTuple = self.callHackage "OneTuple" "0.3" { };
            snap-stream = dontCheck
              (self.callHackage "snap-stream" "0.1.1.0" { });
            tasty = self.callHackage "tasty" "1.3.1" { };
            universe = dontCheck
              (self.callHackage "universe" "1.2" { });
            universe-base = dontCheck
              (self.callHackage "universe-base" "1.1.1" { });
            universe-dependent-sum = dontCheck
              (self.callHackage "universe-dependent-sum" "1.2.0.1" { });
            universe-instances-extended = dontCheck
              (self.callHackage "universe-instances-extended" "1.1.1" { });
            universe-reverse-instances = dontCheck
              (self.callHackage "universe-reverse-instances" "1.1" { });
            universe-some = dontCheck
              (self.callHackage "universe-some" "1.2" { });
            wide-word = dontCheck
              (self.callHackage "wide-word" "0.1.1.2" { });
            generically = dontCheck
              (self.callCabal2nix "generically" srcs.generically { });
            # reflex-dom-forms: not on Hackage
            reflex-dom-forms = self.callCabal2nix "reflex-dom-forms"
              (pkgs.fetchFromGitHub {
                owner = "3noch";
                repo = "reflex-dom-forms";
                rev = "2f7c4a8f80d464f4c0289dfb5aae0c204827592e";
                sha256 = "0s32099nyk7pw44w9nsgi6q1w16f81sd57snc9gj131ymjp9nprv";
              }) { };
          }
      );
  } // projectOverrides)
