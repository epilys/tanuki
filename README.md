# `aarch64` instruction fuzzer

Generate random `aarch64` instructions for fuzzing.

This repository contains two directories:

- [`tanuki-codegen`](./tanuki-codegen): code generator that creates Rust struct types from the `aarch64` specification of the https://gitlab.com/pm215/risu project.
- [`tanuki`](./tanuki): fuzz generator that generates random instructions using the [`arbitrary`](https://crates.io/crates/arbitrary) library.
