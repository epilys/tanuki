# Generate random `aarch64` instructions

Run with `cargo run`

This will generate four files:

1. `TIMESTAMP.seed` the random bytes used as seed, for debugging
2. `TIMESTAMP.bin` the output code
2. `TIMESTAMP.S` the output code disassembled
2. `TIMESTAMP.log` log with one line for each emitted instruction (which struct was used and with what values), for debugging
