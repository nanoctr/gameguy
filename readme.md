# Rustboy
Yet another gameboy emulator written in Rust. 100% WIP, nothing finished yet.

# Building
Building directly via `cargo build` should work just fine.
For development, there's a makefile containing some rules:
  * `make build` (default make rule) runs clippy; this rule requires [cargo clippy](https://github.com/rust-lang/rust-clippy#Usage) to be installed
  * `make run` runs the emulator and enables logging via env variables