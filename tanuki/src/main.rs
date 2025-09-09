// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright Manos Pitsidianakis

use std::fmt::Write;

use arbitrary::{Arbitrary, Unstructured};
use clap::Parser;
use tanuki::aarch64::*;

fn maybe_hex(s: &str) -> Result<u64, String> {
    const HEX_PREFIX: &str = "0x";
    const HEX_PREFIX_UPPER: &str = "0X";
    const HEX_PREFIX_LEN: usize = HEX_PREFIX.len();

    let result = if s.starts_with(HEX_PREFIX) || s.starts_with(HEX_PREFIX_UPPER) {
        u64::from_str_radix(&s[HEX_PREFIX_LEN..], 16)
    } else {
        s.parse::<u64>()
    };

    result.map_err(|err| err.to_string())
}

#[derive(Debug, Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Use existing randomness
    #[arg(long, value_name = "SEED", default_value = None)]
    seed_file: Option<std::path::PathBuf>,
    /// Override output name
    #[arg(long, value_name = "NAME", default_value = None)]
    out_name: Option<String>,
    /// Maximum instruction count to emit
    #[arg(long, value_name = "COUNT", default_value_t = 5000)]
    max_insn: usize,
    /// Start of memory block (if not specified, load/stores will not be
    /// generated)
    #[arg(long, value_name = "HEX", default_value = None, value_parser=maybe_hex)]
    start_of_memory_block: Option<u64>,
    /// Memory block size in bytes
    #[arg(long, value_name = "BYTES", default_value_t = 0x100)]
    memory_block_size: u64,
}

fn main() {
    let cli = Cli::parse();

    let out_name = cli.out_name.unwrap_or_else(|| {
        let mut ts = String::from_utf8(
            std::process::Command::new("date")
                .arg("--iso-8601=seconds")
                .output()
                .expect("failed to execute `date`")
                .stdout,
        )
        .unwrap();
        // Pop newline
        ts.pop();
        ts
    });
    let seed = if let Some(ref seed_file) = cli.seed_file {
        std::fs::read(seed_file).expect("Could not read seed file")
    } else {
        let mut seed = vec![0; 100_000_000];
        getrandom::fill(&mut seed).unwrap();
        std::fs::write(format!("{out_name}.seed"), &seed).unwrap();
        seed
    };

    let mut unstructured = Unstructured::new(&seed);

    let mut ret = vec![];

    tanuki::seed_memory(
        &mut unstructured,
        &mut ret,
        Some(0x400a0000),
        cli.memory_block_size,
    );
    // Initialise scratch registers with random data
    tanuki::write_random_aarch64_regdata(&mut unstructured, &mut ret);

    let mut ctr = 0;

    let mut log_output = String::new();

    // Generate cli.max_insn instructions
    while let Ok(ins) = Instruction::arbitrary(&mut unstructured) {
        ctr += 1;
        let mem_addr = if ins.has_memory() {
            // let Some(mut addr) = cli.start_of_memory_block.clone() else {
            //     continue;
            // };
            let mut addr = 0x400a0000;
            addr += unstructured
                .int_in_range(0..=(cli.memory_block_size - (128 / 8)))
                .unwrap();

            let memops = ins.encode_memops(addr);
            for memop in memops {
                memop.write(&mut ret);
            }
            Some(addr)
        } else {
            None
        };
        tanuki::write_instruction(&ins, &mut ret);
        writeln!(
            &mut log_output,
            "{ctr}={ins:?} {:?} {:08x}",
            ins.group(),
            ins.encode().swap_bytes()
        )
        .unwrap();
        if let Some(mem_addr) = mem_addr {
            tanuki::write_load(mem_addr, &mut ret);
        }
        if ctr > cli.max_insn {
            break;
        } else if ctr.is_multiple_of(100) {
            // Reseed scratch registers
            tanuki::write_random_aarch64_regdata(&mut unstructured, &mut ret);
        }
    }
    tanuki::write_system_off(&mut ret);
    let assembly = tanuki::disas(&ret, 0).unwrap();

    std::fs::write(format!("{out_name}.bin"), &ret).unwrap();
    std::fs::write(format!("{out_name}.S"), assembly.as_bytes()).unwrap();
    std::fs::write(format!("{out_name}.log"), log_output.as_bytes()).unwrap();
}
