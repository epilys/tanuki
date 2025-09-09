// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright Manos Pitsidianakis

use crate::aarch64::IsInstruction;

pub mod aarch64 {
    #![allow(non_camel_case_types)]
    #![allow(clippy::new_without_default)]
    #![allow(clippy::unnecessary_cast)]
    #![allow(clippy::neg_multiply)]
    #![allow(clippy::if_same_then_else)]
    #![allow(clippy::double_parens)]
    #![allow(clippy::nonminimal_bool)]
    #![allow(non_snake_case)]
    #![allow(dead_code)]
    #![allow(unused)]

    use arbitrary::Arbitrary;
    use arbitrary_int::traits::Integer;

    use super::MemoryOp;

    pub trait IsInstruction {
        const GROUP: Option<&str>;

        fn group(&self) -> Option<&'static str> {
            Self::GROUP
        }

        fn has_memory(&self) -> bool;

        fn encode(&self) -> u32;
        fn encode_memops(&self, memory_block: u64) -> Vec<MemoryOp>;
    }

    macro_rules! bitmask {
        ( $off: expr, $len: expr ) => {
            ((1 << $len) - 1) << $off
        };
    }

    macro_rules! setbits {
        ( $var: expr, $off: expr, $len: expr, $val: expr ) => {
            ($var & !bitmask!($off, $len)) | (($val << $off) & bitmask!($off, $len))
        };
    }

    /// sign-extract from a nbit optionally signed bitfield
    macro_rules! sextract {
        ($field:expr, $nbits:expr) => {{
            let field = $field as i64;
            let nbits = $nbits as u32;
            let sign = field & (1 << (nbits - 1));
            ((-1 * sign) + (field ^ sign)) as i64
        }};
    }

    include!(concat!(env!("OUT_DIR"), "/aarch64.rs.inc"));
}
use arbitrary::{Arbitrary, Unstructured};

#[allow(clippy::unusual_byte_groupings)]
const NOP: u32 = 0b110_101_01000000110010_0000_000_11111;

/// Write `u32` as instruction to `$ret`
macro_rules! insn32 {
    ($ret:expr, $insn:expr) => {{
        let insn: u32 = $insn as u32;
        $ret.extend(insn.to_le_bytes().into_iter());
    }};
}

/// Returns bytes as a disassembled string for debugging.
pub fn disas(input: &[u8], starting_address: u64) -> Result<String, Box<dyn std::error::Error>> {
    use std::fmt::Write;

    use capstone::prelude::*;

    let cs = Capstone::new()
        .arm64()
        .mode(capstone::arch::arm64::ArchMode::Arm)
        .endian(capstone::Endian::Little)
        .detail(true)
        .build()
        .expect("Failed to create Capstone object");
    let decoded_iter = cs.disasm_all(input, starting_address)?;
    let mut s = String::new();
    for insn in decoded_iter.as_ref() {
        writeln!(&mut s, "{insn}")?;
    }
    if !s.is_empty() {
        // Remove last newline.
        s.pop();
    }
    Ok(s)
}

pub fn write_movi(rd: u8, imm: i64, ret: &mut Vec<u8>) {
    assert!(rd < 31);
    let highhalf = (imm as u64 >> 16) & 0xffff;

    if imm < 0 {
        // MOVN
        insn32!(
            ret,
            0x92800000 | ((!(imm as u64) & 0xffff) << 5) | rd as u64
        );
        if highhalf != 0xffff {
            // MOVK, LSL 16
            insn32!(ret, 0xf2a00000 | (highhalf << 5) | rd as u64)
        }
    } else {
        // MOVZ
        insn32!(ret, 0x52800000 | (((imm as u64) & 0xffff) << 5) | rd as u64);
        if highhalf != 0 {
            // MOVK, LSL 16
            insn32!(ret, 0xf2a00000 | (highhalf << 5) | rd as u64)
        }
    }
}

pub fn write_movr(rd: u8, rn: u8, ret: &mut Vec<u8>) {
    assert!(rd < 31);
    assert!(rn < 31);

    // using ADD 0x11000000
    insn32!(ret, 0x91000000 | ((rn as u32) << 5) | rd as u32)
}

pub fn write_sub(rd: u8, rn: u8, rm: u8, ret: &mut Vec<u8>) {
    assert!(rd < 31);
    assert!(rn < 31);
    assert!(rm < 31);

    insn32!(
        ret,
        0xcb000000 | ((rm as u32) << 16) | ((rn as u32) << 5) | rd as u32
    );
}

// LSL
pub fn write_subs(rd: u8, rn: u8, rm: u8, mut imm: u32, ret: &mut Vec<u8>) {
    assert!(rd < 31, "{rd}");
    assert!(rn < 31, "{rn}");
    assert!(rm <= 31, "{rm}");
    if imm == 64 {
        imm = 0;
    }

    // LSL
    let lsl = 0;
    insn32!(
        ret,
        0xcb000000
            | (lsl << 22)
            | ((rm as u32) << 16)
            | (imm << 10)
            | ((rn as u32) << 5)
            | rd as u32
    );
}

pub fn seed_memory(
    unstructured: &mut Unstructured<'_>,
    ret: &mut Vec<u8>,
    start_of_memory_block: Option<u64>,
    memory_block_size: u64,
) {
    let Some(start) = start_of_memory_block else {
        return;
    };

    for i in 0..(memory_block_size / 8) {
        let value = <i64>::arbitrary(unstructured).unwrap();
        let mut str = crate::aarch64::STRi_A64::new();
        let addr = start + i * 8;
        write_movi(0, value, ret);
        write_movi(1, addr as i64, ret);
        str.rt = unsafe { <arbitrary_int::u5>::new_unchecked(0) };
        str.rn = unsafe { <arbitrary_int::u5>::new_unchecked(1) };
        insn32!(ret, str.encode());
    }
    insn32!(ret, NOP);
}

pub fn write_load(addr: u64, ret: &mut Vec<u8>) {
    let alignment = 8;
    let addr = addr & !(alignment - 1);
    let mut ldr = crate::aarch64::LDRi_A64::new();
    write_movi(0, addr as i64, ret);
    ldr.rt = unsafe { <arbitrary_int::u5>::new_unchecked(0) };
    ldr.rn = unsafe { <arbitrary_int::u5>::new_unchecked(0) };
    insn32!(ret, ldr.encode());
}

pub fn write_random_aarch64_regdata(unstructured: &mut Unstructured<'_>, ret: &mut Vec<u8>) {
    // clear flags
    insn32!(ret, 0xd51b421f_u32); // msr nzcv, xzr

    // general purpose registers
    for i in 0..31 {
        let value = <i64>::arbitrary(unstructured).unwrap();
        write_movi(i, value, ret);
    }
    insn32!(ret, NOP);
}

pub fn write_instruction(i: &aarch64::Instruction, ret: &mut Vec<u8>) {
    insn32!(ret, i.encode());
}

pub fn write_system_off(ret: &mut Vec<u8>) {
    #[allow(clippy::unusual_byte_groupings)]
    const HVC_0: u32 = 0b110_101_00_000_0000000000000000_000_10;
    const SYSTEM_OFF: i64 = 0x8400_0008;
    write_movi(0, SYSTEM_OFF, ret);
    insn32!(ret, HVC_0);
}

#[derive(Debug, Copy, Clone)]
pub enum MemoryOp {
    MovI {
        rd: u32,
        imm: u64,
    },
    MovR {
        rd: u32,
        rn: u32,
    },
    // sub rd, rn, rm
    Sub {
        rd: u32,
        rn: u32,
        rm: u32,
    },
    // sub rd, rn, rm, shifted
    Subs {
        rd: u32,
        rn: u32,
        rm: u32,
        shift: u32,
    },
}

impl MemoryOp {
    pub fn write(&self, ret: &mut Vec<u8>) {
        match *self {
            Self::MovI { rd, imm } => {
                write_movi(rd.try_into().unwrap(), imm as i64, ret);
            }
            Self::MovR { rd, rn } => {
                write_movr(rd.try_into().unwrap(), rn.try_into().unwrap(), ret);
            }
            Self::Sub { rd, rn, rm } => {
                write_sub(
                    rd.try_into().unwrap(),
                    rn.try_into().unwrap(),
                    rm.try_into().unwrap(),
                    ret,
                );
            }
            Self::Subs { rd, rn, rm, shift } => {
                write_subs(
                    rd.try_into().unwrap(),
                    rn.try_into().unwrap(),
                    rm.try_into().unwrap(),
                    shift,
                    ret,
                );
            }
        }
    }
}
