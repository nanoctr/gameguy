# Requires cargo-clippy
build:
	cargo clippy

release:
	cargo build --release

run: export RUST_LOG = debug
run:
	cargo run