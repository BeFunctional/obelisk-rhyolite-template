# obelisk-rhyolite-template

A template repository for your Obelisk+Rhyolite+React+Tailwind projects.

## What else is included

- [Beam](https://haskell-beam.github.io/beam/) for postgres.
- `haskell-language-server` built from source against the GHC compiler used in this project.  (No segfaults on template haskell!)

## Developing
- Install obelisk
- Run `ob shell` to load the project dependencies onto your PATH.
- This will take a long time to build the necessary dependencies if they don't exist in one of the nix-caches your system is configured to use.
- run your favorite IDE with support for HLS from within the shell.


## Running

- Install obelisk
- Run `ob run` to run the system against a locally provisioned postgresql database and host the app at localhost:8000
- This will take a long time to build the necessary dependencies if they don't exist in one of the nix-caches your system is configured to use.

## Building

* Run `nix-build -A exe` for full server executable.
