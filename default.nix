{ system ? builtins.currentSystem, obelisk ? import ./.obelisk/impl {
  inherit system;
  useGHC810 = true;
  iosSdkVersion = "15.0";
  config.allowBroken = true;
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
    android.applicationId = "obelisk-rhyolite.template";
    android.displayName = "Obelisk+Rhyolite Example";
    ios.bundleIdentifier = "obelisk-rhyolite.template";
    ios.bundleName = "Obelisk+Rhyolite Example";
    staticFiles = import ./static { pkgs = obelisk.nixpkgs; };
    packages = { };

    shellToolOverrides = ghc: super: {
      inherit (pkgs) nixfmt;
      inherit (pkgs.haskellPackages) cabal-plan cabal-fmt;
      haskell-language-server = hls;
    };

    overrides = pkgs.lib.composeExtensions (import (hackGet ./dep/rhyolite) ({
      inherit (args) pkgs;
      inherit obelisk;
      inherit (args.pkgs.darwin.apple_sdk) sdk;
      inherit (args.pkgs.darwin.apple_sdk.frameworks) Cocoa;
    })).haskellOverrides (self: super:
      with pkgs.haskell.lib;
      let
        isGHCJS = self.ghc.isGhcjs or false;
        dontCheckGHCJS = if isGHCJS then dontCheck else x: x;
        dontHaddockGHCJS = if isGHCJS then dontHaddock else x: x;
        githubRepo = fq: rev:
          builtins.fetchTarball
          ("https://github.com/" + fq + "/archive/" + rev + ".tar.gz");
        # A helper function similar to what you've asked for:
        # Takes pkg, ver, sha256, revision and editedCabalFile, then returns
        # a derivation with the specified revision and edited cabal file.
        callHackageDirectRevisionOverride =
          { pkg, ver, sha256, revision, editedCabalFile }:
          compose.overrideCabal (drv: { inherit revision editedCabalFile; })
          (super.callHackageDirect { inherit pkg ver sha256; } { });
        srcs = {
          # Rhyolite needs development versions of vessel
          vessel = githubRepo "obsidiansystems/vessel"
            "a0eafdd20b3844dd23e3b0ac537049c60b9049b0";

          monoid-map = githubRepo "obsidiansystems/monoid-map"
            "620e5353713dd292c33d71462ed0835034612d2f";
          obelisk-oauth = githubRepo "obsidiansystems/obelisk-oauth"
            "d6c04107c90f7195d9c2da93bf726147cc8de61a";

          react = githubRepo "obsidiansystems/react"
            "3aee4ea4c61fbe893491d0e60454b33359e569c3";
          reflex-react = githubRepo "obsidiansystems/reflex-react"
            "f0a59c0d4563eb1365ea8fd1a7c2d501db9db0ca";

        };
      in let
        framesOverrides = {

          # unordered-containers = self.callHackageDirect {
          #   pkg = "unordered-containers";
          #   ver = "0.2.20";
          #   sha256 = "sha256-ZYQ7OsAx/r2Vnk0ZeQANGdtV3DLViC5eV3zW41dydDc=";
          # } { };

          # base-orphans = self.callHackageDirect {
          #   pkg = "base-orphans";
          #   ver = "0.8.8.2";
          #   sha256 = "sha256-QbcpILVWIiOa1cc0qhEpd9/wA4vbFmiN1x0FhoXYSB4=";
          # } { };

          # jsaddle = self.callHackageDirect {
          #   pkg = "jsaddle";
          #   ver = "0.9.8.3";
          #   sha256 = "sha256-BCpVKMP6Q48zLu4V8g1TRSFV4L5Q7Oj79e1hD50sLDA=";
          # } { };

          # vector-binary-instances = callHackageDirectRevisionOverride {
          #   pkg = "vector-binary-instances";
          #   ver = "0.2.5.2";
          #   sha256 = "sha256-b8f6JqdmTkphOcng4hnJNl/ogrLHhvVfsU8uQFHItV4=";
          #   revision = "5";
          #   editedCabalFile =
          #     "sha256-m6jyxalSeCGrR7vZkd17dTO8qmhmLITE8WuHFlURfOs=";
          # };

          # heist = dontCheck (self.callHackageDirect {
          #   pkg = "heist";
          #   ver = "1.1.1.2";
          #   sha256 = "sha256-fBT5djlDuwsHh2OGrzKdu0/Gp72QHaLv+9e+JMcQct0=";
          # } { });

          # vector-sized = self.callHackageDirect {
          #   pkg = "vector-sized";
          #   ver = "1.6.1";
          #   sha256 = "sha256-//EOAwpEEQkdYF88U/bp0uybKleYHRmTWaKsxIZvCeQ=";
          # } { };

          # vector-algorithms = self.callHackageDirect {
          #   pkg = "vector-algorithms";
          #   ver = "0.9.0.3";
          #   sha256 = "sha256-+OtQc7qpzIxRrJNDegfah3Fha8QFgV27c8rPP3iwXkU=";
          # } { };

          # lens = callHackageDirectRevisionOverride {
          #   pkg = "lens";
          #   ver = "5.1.1";
          #   sha256 = "sha256-Nb0tb8iG1nGl2teZ8yICgasOoFMl4uSB/DtM9cIbuqQ=";
          #   revision = "1";
          #   editedCabalFile =
          #     "sha256-xjOkgeab+RHZpu0R7xVoCduLHXx9KW/QMkm5O+OZ46c=";
          # };

          # io-streams = self.callHackageDirect {
          #   pkg = "io-streams";
          #   ver = "1.5.2.2";
          #   sha256 = "sha256-BP+JqXp9w9wwrWVY7lm3g2ElQ0UmkCQ2N+wgeu0YsGs=";
          # } { };

          # vector = self.callHackageDirect {
          #   pkg = "vector";
          #   ver = "0.13.2.0";
          #   sha256 = "sha256-nUS1LHIOkLcsJkli7WOcOAY1tnG6t6mL1By/Bmc87FU=";
          # } { };

          # microstache = self.callHackageDirect {
          #   pkg = "microstache";
          #   ver = "1.0.2.3";
          #   sha256 = "sha256-asZ9Obu4R6R7+xdSMVEm3jnOnMk3PV1K7/3UrCytVvc=";
          # } { };

          # microstache = self.callHackageDirect {
          #   pkg = "microstache";
          #   ver = "1.0.3";
          #   sha256 = "sha256-WfZmtsCPRTWtyQFHDKXy4sbvwZW75UeMqeDqo61plGE=";
          # } { };

          # syb = self.callHackageDirect {
          #   pkg = "syb";
          #   ver = "0.7.2.4";
          #   sha256 = "sha256-a6pO7CDI78cJwtw1EyE9b9OjG6ooCCbFMIjcheu+/d0=";
          # } { };

          # optparse-applicative = dontCheck (callHackageDirectRevisionOverride {
          #   pkg = "optparse-applicative";
          #   ver = "0.18.1.0";
          #   sha256 = "sha256-r1LFJ24uzbpFYGNsQfYPyW9Zjsbu1hcBr0pUekzc73E=";
          #   revision = "1";
          #   editedCabalFile =
          #     "sha256-m6jyxaaSeCGrR7vZkd17dTO8qmhmLITE8WuHFlURfOs=";
          # });

          # criterion-measurement = dontCheck (self.callHackageDirect {
          #   pkg = "criterion-measurement";
          #   ver = "0.2.2.0";
          #   sha256 = "sha256-RMM03eWEOZiGBkskSGEML8MhyttJjXpqz638IjWpP6A=";
          # } { });

          # criterion = dontCheck (self.callHackageDirect {
          #   pkg = "criterion";
          #   ver = "1.6.4.0";
          #   sha256 = "sha256-0L8UN2CZk61WtTjgDv1FHzeLaVFElqEq655DKSV+GTE=";
          # } { });

          # criterion = dontCheck (self.callHackageDirect {
          #   pkg = "criterion";
          #   ver = "1.6.1.0";
          #   sha256 = "sha256-wzKEFR0XMprJOjQZn240x52+HtUsAuyBdfdfvPf0XSQ=";
          # } { });

          # discrimination = self.callHackageDirect {
          #   pkg = "discrimination";
          #   ver = "0.5";
          #   sha256 = "sha256-xB3iS+jvvoR17boBSFUa2RNlmWfpiNjpjqei3XmI8OI=";
          # } { };

          Frames = dontCheck (self.callHackageDirect {
            pkg = "Frames";
            ver = "0.7.4.2";
            sha256 = "sha256-a/nJdGGOcsJAqbZy1mKnu1eFUaE4psAM6+4BW0pnAZ8=";
          } { });
        };
      in {

        vessel = (self.callCabal2nix "vessel" srcs.vessel { });
        monoid-statistics = dontCheck super.monoid-statistics;

        monoid-map = (self.callCabal2nix "monoid-map" srcs.monoid-map { });
        # Needs updating for constraints-extras
        # obelisk-oauth-backend = (self.callCabal2nix "obelisk-oauth-backend" (srcs.obelisk-oauth + "/backend" ){ });
        # obelisk-oauth-common = (self.callCabal2nix "obelisk-oauth-common" (srcs.obelisk-oauth + "/common" ){ });

        react = (self.callCabal2nix "react" srcs.react { });
        reflex-react =
          (self.callCabal2nix "reflex-react" srcs.reflex-react { });

        constraints-extras = self.callHackageDirect {
          pkg = "constraints-extras";
          ver = "0.4.0.1";
          sha256 = "sha256-FV/tNnBR+C8yU9mr8XIEU1v0Wmk+hACOyPsZnHyVi7A=";
        } { };

        patch = self.callHackageDirect {
          pkg = "patch";
          ver = "0.0.8.3";
          sha256 = "sha256-PFggasyTe8jN4T11dbpKI82n3x7bwMu/Mpo1TTOjmhQ=";
        } { };

        postgresql-simple = self.callHackageDirect {
          pkg = "postgresql-simple";
          ver = "0.6.5.1";
          sha256 = "sha256-iwWgIHtO2HQzsDrIOVlNJTJTiB8N1NJMYXHaznRR5mo=";
        } { };
        postgresql-lo-stream = callHackageDirectRevisionOverride {
          pkg = "postgresql-lo-stream";
          ver = "0.1.1.1";
          sha256 = "sha256-kT/Tv4G1TqZV3AbfI6x9JREVsT/z7BmtkJM9v0002UU=";
          revision = "1";
          editedCabalFile =
            "sha256-DMabYDJY5TCpGE+gVTFmZ3hJtfsCMphQNQ2Ivr1aPXw=";
        };
        reflex = self.callHackageDirect {
          pkg = "reflex";
          ver = "0.8.2.2";
          sha256 = "sha256-2PCpkJMGPDi2KdpaItG7xlt9CdgA6w0dwfc7DJ5k5xM=";
        } { };

        dependent-sum-aeson-orphans = callHackageDirectRevisionOverride {
          pkg = "dependent-sum-aeson-orphans";
          ver = "0.3.1.1";
          sha256 = "VUDlfBETZ46jCzTCD41llgJn46AUKGwH+PV2ZAQjJ5I=";
          revision = "2";
          editedCabalFile =
            "sha256-ZUdnRK5CxclpVwzaBCvU054Bwz3qNzIKVo+pZ1n/oPA=";
        };

        beam-postgres = dontCheck (self.callHackageDirect {
          pkg = "beam-postgres";
          ver = "0.5.4.2";
          sha256 = "sha256-0P3ilVmJ2cvWTKPsKf7eivCpILwDscRSQqJ46GD96J8=";
        } { });

        beam-core = self.callHackageDirect {
          pkg = "beam-core";
          ver = "0.10.3.0";
          sha256 = "sha256-lwRV2VlEOQnw/zaQPf9nm8i2jR444YnEwUiuxkvkJDY";
        } { };
        beam-migrate = self.callHackageDirect {
          pkg = "beam-migrate";
          ver = "0.5.3.1";
          sha256 = "l6TFQ69BIRkxoTSh7yarylfcIu5Y3CHanhzBi/fAWpQ=";
        } { };
      } // framesOverrides);
  } // projectOverrides)
