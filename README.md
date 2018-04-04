[![Linux Build Status](https://img.shields.io/travis/cohomology/hlabyrinth/master.svg?label=Linux%20build)](https://travis-ci.org/cohomology/hlabyrinth)  [![Windows Build Status](https://img.shields.io/appveyor/ci/cohomology/hlabyrinth/master.svg?label=Windows%20build)](https://ci.appveyor.com/project/cohomology/hlabyrinth)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Haskell](https://img.shields.io/badge/language-haskell-blue.svg)](https://www.haskell.org)

# hlabyrinth

## 1. Rationale

The is a simple labyrinth builder "game" for my son, written in Haskell and using GTK+ and Cairo. It started as a project to learn [Rust](https://www.rust-lang.org), but it turned out to be written in [Haskell](https://www.haskell.org).

The original Rust implementation can still be found [here](https://github.com/cohomology/rustirinth), but is much less 
complete.

## 2. Game principle

It is possible to build a labyrinth, to place and start and end points and compute the shortest way through the labyrinth. When closed, the current labyrinth is saved and restored, when the game is started.

## 3. Installation

One great source of fun when using Haskell, are the different ways to build a project. There are at least four different ways to build this one, using a disjoint set of tools. I recommend using [stack](https://docs.haskellstack.org) and (optionally) [nix](https://nixos.org/nix/), because it automatically (and locally) installs all dependencies, notably the Haskell compiler `ghc`, `GTK`, `cairo` and `llvm`, without polluting your operating system.

### 3.1 Installation using stack

This installation option has the advantage, that the native operating system libraries are used. Also much less hard disk space is wasted. As a disadvantage, `llvm` can not be used as a `ghc` backend.

1. Install prerequisites

On Debian, this is something like:
```
apt-get install libgirepository1.0-dev libgtk-3-dev libcairo2-dev build-essential cmake
```
2. Install stack
```
curl -sSL https://get.haskellstack.org/ | sh
```
3. Build project
```
stack build
```
### 3.2 Installation using stack and nix

In oposite to the installation option via `stack` alone, this also automatically provides all dependencies in a container like manner. As an additional advantage, one may use the `llvm` backend for `ghc`.

1. Install stack 
```
curl -sSL https://get.haskellstack.org/ | sh
```
[stack](https://docs.haskellstack.org) is nowadays the best way to build Haskell projects. It is like the combination of `rustup` and `cargo` in the Rust world. Especially it automatically installs (eventually multiple) versions of `ghc`, the Haskell compiler.

2. Install nix

```
curl https://nixos.org/nix/install | sh
```
[nix](https://nixos.org/nix/) is like `stack` for _external_ dependencies, like operating system libraries. It ensures that the build is made in some closed container, independent of the environment of the underlying operating system. The `stack.yaml` configuration automatically builds inside a nix container, so you do not have to envoke any `nix` commands by yourself. Note that `nix` uses alot of space inside `/nix/store`. A basic set of packages used for building `hlabyrinth` is about 2 GB.

3. Build the project
```
stack --stack-yaml stack-nix.yaml build --flag hlabyrinth:llvm
```
