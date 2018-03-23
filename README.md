[![Build Status](https://travis-ci.org/cohomology/hlabyrinth.svg?branch=master)](https://travis-ci.org/cohomology/hlabyrinth)

# hlabyrinth

## Rationale

The is a simple labyrinth builder "game" for my son, written in Haskell and using GTK+ and Cairo. It started as a project to learn [Rust](https://www.rust-lang.org), but it turned out to be written in [Haskell](https://www.haskell.org).

The original Rust implementation can still be found [here](https://github.com/cohomology/rustirinth), but is much less 
complete.

## Game principle

It is possible to build a labyrinth, to place and start and end points and compute the shortest way through the labyrinth. When closed, the current labyrinth is saved and restored, when the game is started.

## Installation

One great source of fun when using Haskell, are the different ways to build a project. There are at least four different ways to build this one, using a disjoint set of tools. I recommend using [stack](https://docs.haskellstack.org) and [nix](https://nixos.org/nix/), because it automatically (and locally) installs all dependencies, notably the Haskell compiler `ghc`, `GTK`, `cairo` and `llvm`, without polluting your operating system.

### Installation using stack and nix

1. Install stack 

```
curl -sSL https://get.haskellstack.org/ | sh
```
[stack](https://docs.haskellstack.org) is nowadays the best way to build Haskell projects. It is like the combination of `rustup` and `cargo` in the Rust world. Especially it automatically installs (eventually multiple) versions of `ghc`, the Haskell compiler.

2. Install nix

```
curl https://nixos.org/nix/install | sh
```
[nix](https://nixos.org/nix/) is like `stack` for _external_ dependencies, like operating system libraries. It ensures that the build is made in some closed container, independent of the environment of the underlying operating system. The `stack.yaml` configuration automatically builds inside a nix container, so you do not have to envoke any `nix` commands by yourself. 

3. Build the project
```
stack build
```
4. Run the project
```
stack install stack-run
stack run
```
