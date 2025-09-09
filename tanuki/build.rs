// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright Manos Pitsidianakis

use std::io::Write;

use tanuki_codegen::*;

fn main() {
    println!("cargo::rerun-if-changed=../aarch64.risu");

    let risu = std::fs::read_to_string("../aarch64.risu").unwrap();
    let mut input = risu.as_str();
    let mut instructions = parse(&mut input).unwrap();
    instructions.retain(|i| {
        i.encodingname == "A64"
            && !i.insnname.contains("_RES")
            && !i.insnname.contains("CRC32")
            && i.group.as_ref().map(|g| !g.contains("v8")).unwrap_or(true)
    });
    assert_eq!(input, "");
    //eprintln!("{instructions:#?}");
    let target_dir = std::env::var("OUT_DIR").unwrap();
    let out = format!("{target_dir}/aarch64.rs.inc");
    let mut out = std::fs::File::create(&out).unwrap();
    writeln!(&mut out, "// @generated\n").unwrap();
    let mut enum_ = vec![];
    for i in instructions {
        enum_.push(i.ident());
        writeln!(&mut out, "{}", i.codegen()).unwrap();
    }
    writeln!(
        &mut out,
        "#[derive(Arbitrary, Debug, Copy, Clone)]\npub enum Instruction {{"
    )
    .unwrap();
    for i in &enum_ {
        writeln!(&mut out, "   {i}({i}),").unwrap();
    }
    writeln!(&mut out, "}}").unwrap();
    writeln!(
        &mut out,
        r#"
    impl IsInstruction for Instruction {{
        const GROUP: Option<&str> = None;

        fn has_memory(&self) -> bool {{
            match self {{"#
    )
    .unwrap();
    for i in &enum_ {
        writeln!(&mut out, "   Self::{i}(inner) => inner.has_memory(),").unwrap();
    }
    writeln!(
        &mut out,
        r#"
            }}
        }}
        fn encode(&self) -> u32 {{
            match self {{"#
    )
    .unwrap();
    for i in &enum_ {
        writeln!(&mut out, "   Self::{i}(inner) => inner.encode(),").unwrap();
    }
    writeln!(
        &mut out,
        r#"
            }}
        }}

        fn encode_memops(&self, memory_block: u64) -> Vec<MemoryOp> {{
            match self {{
"#
    )
    .unwrap();
    for i in &enum_ {
        writeln!(
            &mut out,
            "   Self::{i}(inner) => inner.encode_memops(memory_block),"
        )
        .unwrap();
    }
    writeln!(
        &mut out,
        r#"
            }}
        }}
    }}
"#
    )
    .unwrap();
    out.flush().unwrap();
}
