# Simple Parser in Haskell

A simple parser for a Haskell-like language written in Haskell.

## Table of Contents
1. [Installation](#installation)
2. [Usage](#usage)
3. [License](#license)

## Installation

### Using Nix-Shell (Optional)

1. Follow the [Nix installation instructions](https://nixos.org/download/) to set up `nix`.

2. Enter the nix shell from the `shell.nix`.

```bash
nix-shell
```

3. Build the project using ghc or run it with runhaskell. 

### Using ghc

```bash
ghc Main.hs
./Main
```

### Using runhaskell

```bash
runhaskell Main.hs
```

## Usage

You can modify the `test/Main.2hs` file and run the project to generate the abstract syntax tree of the file contents.

> [!NOTE]
> Not all features of Haskell are supported.

## License

This project is licensed under the [MIT License](https://choosealicense.com/licenses/mit/). Feel free to modify and use it as you wish!