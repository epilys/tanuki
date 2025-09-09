// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright Manos Pitsidianakis

mod constraints;
mod memory;

use constraints::Constraint;
use memory::Memory;
use quote::quote;
use winnow::{
    ascii::{line_ending, multispace1, till_line_ending},
    combinator::{delimited, opt},
    error::{ParserError, Result},
    prelude::*,
    stream::Stream,
    token::{literal, take_till},
};

#[derive(Debug)]
pub enum Bitfield {
    Constant(String),
    Named { identifier: String, bitlength: u8 },
}

#[derive(Debug)]
pub struct Instruction {
    pub insnname: String,
    pub encodingname: String,
    pub group: Option<String>,
    pub bitfields: Vec<Bitfield>,
    pub constraints: Vec<Constraint>,
    pub memory: Option<Memory>,
}

impl Instruction {
    pub fn ident(&self) -> syn::Ident {
        let Self {
            insnname,
            encodingname,
            ..
        } = self;
        quote::format_ident!("{insnname}_{encodingname}")
    }

    pub fn codegen(&self) -> String {
        let Self {
            insnname,
            encodingname,
            bitfields,
            constraints,
            group,
            memory,
            ..
        } = self;
        let insnname = quote::format_ident!("{insnname}_{encodingname}");
        let mut fields = vec![];

        let mut bit_no = 32;

        let mut const_field_no = 0;
        let mut const_field_inits = vec![];
        let mut arbitrary_field_inits = vec![];
        let mut verify_field_destructure = vec![];
        let mut arbitrary_field_accepts = vec![];
        let mut encode_field = vec![];
        for f in bitfields.iter() {
            match f {
                Bitfield::Named {
                    identifier,
                    bitlength: 8,
                } => {
                    let f = quote::format_ident!("{identifier}");
                    const_field_inits.push(quote! {
                        #f: 0
                    });
                    verify_field_destructure.push(quote! {
                        #f,
                    });
                    arbitrary_field_inits.push(quote! {
                        let #f: u8 =  u.arbitrary()?;
                    });
                    arbitrary_field_accepts.push(quote! {
                        ret.#f = #f;
                    });
                    bit_no -= 8;
                    encode_field.push(quote! {
                        {
                            let val = self.#f as u32;
                            accum = setbits!(accum, #bit_no, 8, val);
                        }
                    });
                    let ty = quote! { pub #f: u8 };
                    fields.push(ty);
                }
                Bitfield::Named {
                    identifier,
                    bitlength: 16,
                } => {
                    let f = quote::format_ident!("{identifier}");
                    const_field_inits.push(quote! {
                        #f: 0
                    });
                    verify_field_destructure.push(quote! {
                        #f,
                    });
                    arbitrary_field_inits.push(quote! {
                        let #f: u16 =  u.arbitrary()?;
                    });
                    arbitrary_field_accepts.push(quote! {
                        ret.#f = #f;
                    });
                    bit_no -= 16;
                    encode_field.push(quote! {
                        {
                            let val = self.#f as u32;
                            accum = setbits!(accum, #bit_no, 16, val);
                        }
                    });
                    let ty = quote! { pub #f: u16 };
                    fields.push(ty);
                }
                Bitfield::Named {
                    identifier,
                    bitlength,
                } => {
                    let f = quote::format_ident!("{identifier}");
                    let ty: syn::Type =
                        syn::parse_str(&format!("arbitrary_int::u{bitlength}")).unwrap();
                    const_field_inits.push(quote! {
                        #f: unsafe { <#ty>::new_unchecked(0) }
                    });
                    verify_field_destructure.push(quote! {
                        #f,
                    });
                    arbitrary_field_inits.push(quote! {
                        let #f: #ty =  u.arbitrary()?;
                    });
                    arbitrary_field_accepts.push(quote! {
                        ret.#f = #f;
                    });
                    bit_no -= bitlength;
                    encode_field.push(quote! {
                        {
                            let val = self.#f.as_u32();
                            accum = setbits!(accum, #bit_no, #bitlength, val);
                        }
                    });
                    let ty = quote! { pub #f: #ty };
                    fields.push(ty);
                }
                Bitfield::Constant(val) if val.len() == 8 => {
                    let f = quote::format_ident!("_const_{const_field_no}");
                    const_field_no += 1;
                    let val: syn::LitInt = syn::parse_str(&format!("0b{val}")).unwrap();
                    const_field_inits.push(quote! {
                        #f: #val
                    });
                    verify_field_destructure.push(quote! {
                        #f,
                    });
                    bit_no -= 8;
                    encode_field.push(quote! {
                        {
                            let val = self.#f as u32;
                            accum = setbits!(accum, #bit_no, 8, val);
                        }
                    });
                    let ty = quote! { pub #f: u8 };
                    fields.push(ty);
                }
                Bitfield::Constant(val) => {
                    let bitlength = val.len();
                    let f = quote::format_ident!("_const_{const_field_no}");
                    const_field_no += 1;
                    let ty: syn::Type =
                        syn::parse_str(&format!("arbitrary_int::u{bitlength}")).unwrap();

                    let val: syn::LitInt = syn::parse_str(&format!("0b{val}")).unwrap();
                    const_field_inits.push(quote! {
                        #f: unsafe { <#ty>::new_unchecked(#val) }
                    });
                    verify_field_destructure.push(quote! {
                        #f,
                    });
                    bit_no -= bitlength as u8;
                    encode_field.push(quote! {
                        {
                            let val = self.#f.as_u32();
                            accum = setbits!(accum, #bit_no, #bitlength, val);
                        }
                    });
                    let ty = quote! { pub #f: #ty };
                    fields.push(ty);
                }
            }
        }
        assert_eq!(bit_no, 0);
        let mut constraint_checks = vec![];
        for c in constraints.iter() {
            let expr = c.to_rust();
            let comment = &c.original;
            constraint_checks.push(quote! {
                const _DOC: &str = #comment;
                if !(#expr) {
                    return false;
                }
            });
        }
        let mut memops = quote! {};
        let mut has_memory = false;
        if let Some(memory) = memory {
            has_memory = true;
            memops = memory.to_rust();
        }

        let group = if let Some(group) = group {
            quote! { Some(#group) }
        } else {
            quote! { None }
        };

        let struct_def = quote! {
            #[derive(Debug, Copy, Clone)]
            pub struct #insnname {
                #(#fields),*
            }


            impl IsInstruction for #insnname {
                const GROUP: Option<&str> = #group;

                fn has_memory(&self) -> bool {
                    #has_memory
                }

                fn encode(&self) -> u32 {
                    let mut accum = 0_u32;
                    #(#encode_field)*
                    accum
                }

                fn encode_memops(&self, memory_block: u64) -> Vec<MemoryOp> {
                    // Default alignment
                    let mut alignment = 16;
                    let mut ops = vec![];
                    #memops
                    ops
                }
            }

            impl #insnname {
                pub const fn new() -> Self {
                    Self {
                        #(#const_field_inits),*
                    }
                }

                pub fn verify_constraints(&self) -> bool {
                    let Self {
                        #(#verify_field_destructure)*
                    } = self;

                    #(#constraint_checks)*

                    true
                }
            }

            impl<'a> arbitrary::Arbitrary<'a> for #insnname
                {
                    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
                        let mut ret = Self::new();

                        u.arbitrary_loop(None, None, |u| {
                            #(#arbitrary_field_inits);*
                            #(#arbitrary_field_accepts);*

                            if !ret.verify_constraints() {
                                return Ok(std::ops::ControlFlow::Continue(()));
                            }

                            Ok(std::ops::ControlFlow::Break(()))
                        })?;

                        Ok(ret)
                    }
                }
        };
        let output = quote! {
            #struct_def
        };
        let syntax_tree = syn::parse2(output).unwrap();
        prettyplease::unparse(&syntax_tree)
    }
}

#[derive(Debug)]
enum Line {
    Ignored,
    Group(Option<String>),
    Instruction(Instruction),
    Constraint(String),
    Memory(String),
    Directive(String),
}

pub fn parse_prefix(input: &mut &str) -> Result<char> {
    let c = input
        .next_token()
        .ok_or_else(|| ParserError::from_input(input))?;
    if c != '0' {
        return Err(ParserError::from_input(input));
    }
    Ok(c)
}

fn sanitize_constraint(mut input: String) -> Constraint {
    if input.ends_with(";") {
        input = input.strip_suffix(";").unwrap().to_string();
    }
    Constraint::parse(&input)
}

fn sanitize_identifier(input: String) -> String {
    if input == "type" {
        "r#type".to_string()
    } else {
        input
    }
}

fn parse_bitfield(input: &str) -> Result<Bitfield> {
    if input.contains(":") {
        let mut s = input.split(":");
        let identifier = sanitize_identifier(s.next().unwrap().to_string());
        let bitlength = s.next().unwrap().parse().unwrap();
        Ok(Bitfield::Named {
            identifier,
            bitlength,
        })
    } else if <u16>::from_str_radix(input.trim(), 2).is_ok() {
        Ok(Bitfield::Constant(input.to_string()))
    } else {
        Ok(Bitfield::Named {
            identifier: sanitize_identifier(input.into()),
            bitlength: 1,
        })
    }
}

fn parse_instruction(input: &mut &str) -> Result<Instruction> {
    let insnname = take_till(0.., |c| c == ' ').parse_next(input)?.to_string();
    multispace1.parse_next(input)?;
    let encodingname = take_till(0.., |c| c == ' ').parse_next(input)?.to_string();
    multispace1.parse_next(input)?;
    let mut bitfields = vec![];
    let mut constraints = vec![];
    let mut memory = vec![];

    let mut tokens = input.split_whitespace().peekable();
    while !matches!(
        tokens.peek(),
        None | Some(&"!constraints") | Some(&"!memory")
    ) {
        let bitfield = tokens.next().unwrap();
        bitfields.push(parse_bitfield(bitfield)?);
    }

    while tokens.peek().is_some() {
        if matches!(tokens.peek(), Some(&"!constraints")) {
            _ = tokens.next();
            assert_eq!(tokens.next(), Some("{"));
            let mut curr = String::new();
            while !matches!(tokens.peek(), None | Some(&"}")) {
                let c = tokens.next().unwrap();
                if !curr.is_empty() {
                    curr.push(' ');
                }
                curr.push_str(c);
                if curr.ends_with(";") {
                    constraints.push(sanitize_constraint(std::mem::take(&mut curr)));
                }
            }
            if !curr.trim().is_empty() {
                constraints.push(sanitize_constraint(std::mem::take(&mut curr)));
            }
            assert_eq!(tokens.next(), Some("}"));
        } else if matches!(tokens.peek(), Some(&"!memory")) {
            _ = tokens.next();
            assert_eq!(tokens.next(), Some("{"));
            let mut curr = String::new();
            while !matches!(tokens.peek(), None | Some(&"}")) {
                let c = tokens.next().unwrap();
                if !curr.is_empty() {
                    curr.push(' ');
                }
                curr.push_str(c);
                if curr.ends_with(";") {
                    memory.push(std::mem::take(&mut curr));
                }
            }
            if !curr.trim().is_empty() {
                memory.push(std::mem::take(&mut curr));
            }
            assert_eq!(tokens.next(), Some("}"));
        } else {
            panic!("{:?}", tokens.collect::<Vec<&str>>());
        }
    }

    let memory = if memory.is_empty() {
        None
    } else {
        Some(Memory::parse(&memory.join("")))
    };
    Ok(Instruction {
        insnname,
        encodingname,
        group: None,
        bitfields,
        constraints,
        memory,
    })
}

fn parse_line(input: &mut &str) -> Result<Line> {
    let mut line_s = till_line_ending.parse_next(input)?.to_string();
    line_ending.parse_next(input)?;
    while line_s.ends_with("\\") {
        line_s.pop();
        line_s.push_str(till_line_ending.parse_next(input)?);
        line_ending.parse_next(input)?;
    }
    let mut line = line_s.as_str();
    if let Some(pos) = line.find('#') {
        line = &line[..pos];
    }
    if line.starts_with('#') || line.trim().is_empty() {
        Ok(Line::Ignored)
    } else if line.starts_with('.') {
        Ok(Line::Directive(line.into()))
    } else if let Some(group) = line.strip_prefix('@') {
        let group = group.trim();
        let group = if group.is_empty() {
            None
        } else {
            Some(group.to_string())
        };
        Ok(Line::Group(group))
    } else if line.starts_with("!constraints") {
        literal("!constraints ").parse_next(&mut line)?;
        let block = delimited("{", take_till(0.., |c| c == '}'), "}").parse_next(&mut line)?;
        opt(multispace1).parse_next(&mut line)?;
        Ok(Line::Constraint(block.to_string()))
    } else if line.starts_with("!memory") {
        literal("!memory ").parse_next(&mut line)?;
        let block = delimited("{", take_till(0.., |c| c == '}'), "}").parse_next(&mut line)?;
        Ok(Line::Memory(block.to_string()))
    } else if let Ok(i) = parse_instruction.parse_next(&mut line) {
        Ok(Line::Instruction(i))
    } else {
        panic!("{line:?}");
    }
}

pub fn parse(input: &mut &str) -> Result<Vec<Instruction>> {
    let mut instructions = vec![];
    let mut last_group = None;
    while !input.is_empty() {
        match parse_line.parse_next(input)? {
            Line::Ignored => {}
            Line::Group(group) => {
                last_group = group;
            }
            Line::Instruction(mut i) => {
                i.group = last_group.clone();
                instructions.push(i);
            }
            Line::Constraint(c) => {
                // FIXME: remove
                unreachable!("{c}");
            }
            Line::Memory(m) => {
                // FIXME: remove
                unreachable!("{m}");
            }
            Line::Directive(d) => {
                eprintln!("directive: {d}");
            }
        }
    }
    Ok(instructions)
}

#[test]
fn test_parse() {
    const INPUT: &str = r#"STRBr A64 00 111000 00 1 rm:5 011 shft:1 10 rn:5 rt:5 \
!constraints { $rn != 31 && $rn != $rt && $rm != $rt && $rn != $rm; } \
!memory { align(1); reg_plus_reg_shifted($rn, $rm, $shft ? 0 : 0); }
"#;

    let mut input = INPUT;

    let mut instructions = parse(&mut input).unwrap();
    assert_eq!(instructions.len(), 1);
    let strbr = instructions.pop().unwrap();
    assert_eq!(strbr.codegen(), {
        let expected = quote! {
            #[derive(Debug, Copy, Clone)]
            pub struct STRBr_A64 {
                pub _const_0: arbitrary_int::u2,
                pub _const_1: arbitrary_int::u6,
                pub _const_2: arbitrary_int::u2,
                pub _const_3: arbitrary_int::u1,
                pub rm: arbitrary_int::u5,
                pub _const_4: arbitrary_int::u3,
                pub shft: arbitrary_int::u1,
                pub _const_5: arbitrary_int::u2,
                pub rn: arbitrary_int::u5,
                pub rt: arbitrary_int::u5,
            }

            impl IsInstruction for STRBr_A64 {
                const GROUP: Option<&str> = None;
                fn has_memory(&self) -> bool {
                    true
                }
                fn encode(&self) -> u32 {
                    let mut accum = 0_u32;
                    {
                        let val = self._const_0.as_u32();
                        accum = setbits!(accum, 30u8, 2usize, val);
                    }
                    {
                        let val = self._const_1.as_u32();
                        accum = setbits!(accum, 24u8, 6usize, val);
                    }
                    {
                        let val = self._const_2.as_u32();
                        accum = setbits!(accum, 22u8, 2usize, val);
                    }
                    {
                        let val = self._const_3.as_u32();
                        accum = setbits!(accum, 21u8, 1usize, val);
                    }
                    {
                        let val = self.rm.as_u32();
                        accum = setbits!(accum, 16u8, 5u8, val);
                    }
                    {
                        let val = self._const_4.as_u32();
                        accum = setbits!(accum, 13u8, 3usize, val);
                    }
                    {
                        let val = self.shft.as_u32();
                        accum = setbits!(accum, 12u8, 1u8, val);
                    }
                    {
                        let val = self._const_5.as_u32();
                        accum = setbits!(accum, 10u8, 2usize, val);
                    }
                    {
                        let val = self.rn.as_u32();
                        accum = setbits!(accum, 5u8, 5u8, val);
                    }
                    {
                        let val = self.rt.as_u32();
                        accum = setbits!(accum, 0u8, 5u8, val);
                    }
                    accum
                }
                fn encode_memops(&self, memory_block: u64) -> Vec<MemoryOp> {
                    let mut alignment = 16;
                    let mut ops = vec![];
                    alignment = 1u32;
                    let base = u32::from(self.rn);
                    let mut idx = u32::from(self.rm);
                    let shift = if u32::from(self.shft) != 0u32 {
                        0i64
                    } else {
                        0i64
                    };
                    if shift > 4 {
                        panic!("reg_plus_reg_shifted: bad shift size {shift}");
                    }
                    let mut saveidx = false;

                    if idx == 0 {
                        saveidx = true;
                        idx = 1;
                        if idx == base {
                            idx += 1;
                        }
                        ops.push(MemoryOp::MovR { rd: idx, rn: 0 });
                    }

                    ops.push(MemoryOp::MovI {
                        rd: 0,
                        imm: memory_block & !(alignment - 1) as u64,
                    });
                    ops.push(MemoryOp::Subs {
                        rd: base,
                        rn: 0,
                        rm: idx,
                        shift: shift as _,
                    });
                    if saveidx {
                        ops.push(MemoryOp::MovR { rd: 0, rn: idx });
                    } else if base != 0 {
                        ops.push(MemoryOp::MovI { rd: 0, imm: 0 });
                    }
                    ops
                }
            }
            impl STRBr_A64 {
                pub const fn new() -> Self {
                    Self {
                        _const_0: unsafe { <arbitrary_int::u2>::new_unchecked(0b00) },
                        _const_1: unsafe { <arbitrary_int::u6>::new_unchecked(0b111000) },
                        _const_2: unsafe { <arbitrary_int::u2>::new_unchecked(0b00) },
                        _const_3: unsafe { <arbitrary_int::u1>::new_unchecked(0b1) },
                        rm: unsafe { <arbitrary_int::u5>::new_unchecked(0) },
                        _const_4: unsafe { <arbitrary_int::u3>::new_unchecked(0b011) },
                        shft: unsafe { <arbitrary_int::u1>::new_unchecked(0) },
                        _const_5: unsafe { <arbitrary_int::u2>::new_unchecked(0b10) },
                        rn: unsafe { <arbitrary_int::u5>::new_unchecked(0) },
                        rt: unsafe { <arbitrary_int::u5>::new_unchecked(0) },
                    }
                }
                pub fn verify_constraints(&self) -> bool {
                    let Self {
                        _const_0,
                        _const_1,
                        _const_2,
                        _const_3,
                        rm,
                        _const_4,
                        shft,
                        _const_5,
                        rn,
                        rt,
                    } = self;
                    const _DOC: &str = "$rn != 31 && $rn != $rt && $rm != $rt && $rn != $rm";
                    if !(((((rn.as_u32() != 31u32) && (rn.as_u32() != rt.as_u32()))
                                && (rm.as_u32() != rt.as_u32())) && (rn.as_u32() != rm.as_u32())))
                    {
                        return false;
                    }
                    true
                }
            }
            impl<'a> arbitrary::Arbitrary<'a> for STRBr_A64 {
                fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
                    let mut ret = Self::new();
                    u.arbitrary_loop(
                        None,
                        None,
                        |u| {
                            let rm: arbitrary_int::u5 = u.arbitrary()?;
                            let shft: arbitrary_int::u1 = u.arbitrary()?;
                            let rn: arbitrary_int::u5 = u.arbitrary()?;
                            let rt: arbitrary_int::u5 = u.arbitrary()?;
                            ret.rm = rm;
                            ret.shft = shft;
                            ret.rn = rn;
                            ret.rt = rt;
                            if !ret.verify_constraints() {
                                return Ok(std::ops::ControlFlow::Continue(()));
                            }
                            Ok(std::ops::ControlFlow::Break(()))
                        },
                    )?;
                    Ok(ret)
                }
            }
        };
        let syntax_tree = syn::parse2(expected).unwrap();
        prettyplease::unparse(&syntax_tree)
    });
}
