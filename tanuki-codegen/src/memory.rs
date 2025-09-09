// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright Manos Pitsidianakis

use std::collections::VecDeque;

use quote::quote;

#[derive(Debug, PartialEq)]
pub enum ArithmExprTerminal {
    Sextract(String, u32),
    Field(String),
    Literal(i64),
}

impl ArithmExprTerminal {
    fn to_rust(&self) -> proc_macro2::TokenStream {
        match self {
            Self::Sextract(f, i) => {
                let f = quote::format_ident!("{f}");
                quote! { sextract!(u32::from(self.#f), #i) }
            }
            Self::Field(f) => {
                let f = quote::format_ident!("{f}");
                quote! { u32::from(self.#f) }
            }
            Self::Literal(i) => {
                quote! { #i }
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Comparison {
    Not(String, u32),
    Equals(String, u32),
}

impl Comparison {
    fn to_rust(&self) -> proc_macro2::TokenStream {
        match self {
            Self::Not(f, i) => {
                let f = quote::format_ident!("{f}");
                quote! { u32::from(self.#f) != #i }
            }
            Self::Equals(f, i) => {
                let f = quote::format_ident!("{f}");
                quote! { u32::from(self.#f) == #i }
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ArithmExpr {
    Terminal(ArithmExprTerminal),
    Multiplied(ArithmExprTerminal, ArithmExprTerminal),
    Ternary(Comparison, Box<ArithmExpr>, Box<ArithmExpr>),
}

impl ArithmExpr {
    fn to_rust(&self) -> proc_macro2::TokenStream {
        let mut accum = quote! {};
        match self {
            Self::Terminal(t) => {
                let t = t.to_rust();
                accum = quote! { #accum #t };
            }
            Self::Multiplied(t1, t2) => {
                let t1 = t1.to_rust();
                let t2 = t2.to_rust();
                accum = quote! { #accum (#t1 as i64) * (#t2 as i64) };
            }
            Self::Ternary(comp, then, orelse) => {
                let comp = comp.to_rust();
                let then = then.to_rust();
                let orelse = orelse.to_rust();
                accum = quote! { #accum if #comp { #then } else { #orelse } };
            }
        }
        accum
    }
}

#[derive(Debug, PartialEq)]
pub enum MemoryExpr {
    AlignImm(u32),
    AlignShiftedImm(u32, String),
    Reg(String),
    RegPlusImm(String, ArithmExpr),
    RegPlusRegShifted(String, String, ArithmExpr),
}

fn parse_expr_terminal(lex: &mut VecDeque<Tok>) -> ArithmExprTerminal {
    match lex.pop_front().expect("EOF") {
        Tok::Field(f) => ArithmExprTerminal::Field(f),
        Tok::Integer(i) => ArithmExprTerminal::Literal(i as i64),
        Tok::Sextract => {
            assert_eq!(lex.pop_front(), Some(Tok::LeftParenthesis));
            let Tok::Field(field) = lex.pop_front().unwrap() else {
                unreachable!()
            };
            assert_eq!(lex.pop_front(), Some(Tok::Comma));
            let Tok::Integer(i) = lex.pop_front().unwrap() else {
                unreachable!()
            };
            assert_eq!(lex.pop_front(), Some(Tok::RightParenthesis));
            ArithmExprTerminal::Sextract(field, i)
        }
        other => panic!("{other:?}"),
    }
}

fn parse_ternary(lex: &mut VecDeque<Tok>) -> ArithmExpr {
    let Tok::Field(field) = lex.pop_front().unwrap() else {
        unreachable!()
    };
    let comp = match lex.pop_front().unwrap() {
        Tok::Equals => {
            let Tok::Integer(i) = lex.pop_front().unwrap() else {
                unreachable!()
            };
            assert_eq!(lex.pop_front(), Some(Tok::QuestionMark));
            Comparison::Equals(field, i)
        }
        Tok::QuestionMark => Comparison::Not(field, 0),
        other => panic!("{other:?}"),
    };

    let then = Box::new(parse_expr(lex));
    assert_eq!(lex.pop_front(), Some(Tok::Colon));
    let orelse = Box::new(parse_expr(lex));
    ArithmExpr::Ternary(comp, then, orelse)
}

fn parse_expr(lex: &mut VecDeque<Tok>) -> ArithmExpr {
    match (lex.front().expect("EOF"), lex.get(1), lex.get(2)) {
        (Tok::Field(_), Some(Tok::Equals), _) => parse_ternary(lex),
        (Tok::Field(_), Some(Tok::QuestionMark), _) => parse_ternary(lex),
        (Tok::Sextract, _, _) => {
            let n = parse_expr_terminal(lex);
            if matches!(lex.front(), Some(Tok::Comma | Tok::RightParenthesis)) {
                return ArithmExpr::Terminal(n);
            }
            if matches!(lex.front(), Some(Tok::Multiplication)) {
                lex.pop_front();
                let m = parse_expr_terminal(lex);
                return ArithmExpr::Multiplied(n, m);
            }
            panic!("{lex:?}");
        }
        (Tok::Field(_), Some(Tok::Multiplication), _) => {
            let n = parse_expr_terminal(lex);
            lex.pop_front();
            let m = parse_expr_terminal(lex);
            ArithmExpr::Multiplied(n, m)
        }
        (Tok::Integer(_), _, _) => {
            let Tok::Integer(i) = lex.pop_front().unwrap() else {
                unreachable!()
            };
            ArithmExpr::Terminal(ArithmExprTerminal::Literal(i as i64))
        }
        (Tok::Field(_), _, _) => {
            let Tok::Field(f) = lex.pop_front().unwrap() else {
                unreachable!()
            };
            ArithmExpr::Terminal(ArithmExprTerminal::Field(f))
        }
        other => panic!("{other:?}"),
    }
}

fn parse_align(lex: &mut VecDeque<Tok>, ret: &mut Vec<MemoryExpr>) {
    assert_eq!(lex.pop_front(), Some(Tok::LeftParenthesis));
    match (lex.front(), lex.get(1), lex.get(2)) {
        (Some(Tok::Integer(_)), Some(Tok::ShiftLeft), Some(Tok::Field(_))) => {
            let Tok::Integer(imm) = lex.pop_front().unwrap() else {
                unreachable!()
            };
            lex.pop_front();
            let Tok::Field(field) = lex.pop_front().unwrap() else {
                unreachable!()
            };
            ret.push(MemoryExpr::AlignShiftedImm(imm, field));
        }
        (Some(Tok::Integer(imm)), _, _) => {
            ret.push(MemoryExpr::AlignImm(*imm));
            lex.pop_front();
        }
        other => panic!("{other:?}"),
    }
    assert_eq!(lex.pop_front(), Some(Tok::RightParenthesis));
}

#[derive(Debug)]
pub struct Memory {
    exprs: Vec<MemoryExpr>,
}

impl Memory {
    pub fn parse(mut s: &str) -> Self {
        eprintln!("Parsing {s:?}");
        let mut lexer_tokens = VecDeque::new();
        macro_rules! ws {
            () => {{
                if s.chars().nth(0).unwrap().is_whitespace() {
                    s = &s[1..];
                    continue;
                }
            }};
        }

        while !s.is_empty() {
            ws!();
            if s.starts_with("align(") {
                lexer_tokens.push_back(Tok::Align);
                s = &s["align".len()..];
            } else if s.starts_with("reg(") {
                lexer_tokens.push_back(Tok::Reg);
                s = &s["reg".len()..];
            } else if s.starts_with("reg_plus_imm(") {
                lexer_tokens.push_back(Tok::RegPlusImm);
                s = &s["reg_plus_imm".len()..];
            } else if s.starts_with("reg_plus_reg_shifted(") {
                lexer_tokens.push_back(Tok::RegPlusRegShifted);
                s = &s["reg_plus_reg_shifted".len()..];
            } else if s.starts_with("sextract(") {
                lexer_tokens.push_back(Tok::Sextract);
                s = &s["sextract".len()..];
            } else if s.starts_with("$") {
                let mut end = 1;
                while s.as_bytes()[end].is_ascii_alphabetic() {
                    end += 1;
                }
                lexer_tokens.push_back(Tok::Field(s[1..end].to_string()));
                s = &s[end..];
            } else if s.starts_with(",") {
                lexer_tokens.push_back(Tok::Comma);
                s = &s[1..];
            } else if s.starts_with("*") {
                lexer_tokens.push_back(Tok::Multiplication);
                s = &s[1..];
            } else if s.starts_with("<<") {
                lexer_tokens.push_back(Tok::ShiftLeft);
                s = &s[2..];
            } else if s.starts_with("==") {
                lexer_tokens.push_back(Tok::Equals);
                s = &s[2..];
            } else if s.starts_with("?") {
                lexer_tokens.push_back(Tok::QuestionMark);
                s = &s[1..];
            } else if s.starts_with(":") {
                lexer_tokens.push_back(Tok::Colon);
                s = &s[1..];
            } else if s.starts_with("(") {
                lexer_tokens.push_back(Tok::LeftParenthesis);
                s = &s[1..];
            } else if s.starts_with(")") {
                lexer_tokens.push_back(Tok::RightParenthesis);
                s = &s[1..];
            } else if s.starts_with(";") {
                s = &s[1..];
            } else if s.as_bytes()[0].is_ascii_digit() {
                let mut end = 1;
                while s.as_bytes()[end].is_ascii_digit() {
                    end += 1;
                }
                lexer_tokens.push_back(Tok::Integer(s[..end].parse::<u32>().unwrap()));
                s = &s[end..];
            } else {
                panic!("{s}");
            }
        }
        dbg!(&lexer_tokens);

        let mut ret = vec![];
        while let Some(tok) = lexer_tokens.pop_front() {
            match tok {
                Tok::Align => parse_align(&mut lexer_tokens, &mut ret),
                Tok::Reg => {
                    assert_eq!(lexer_tokens.pop_front(), Some(Tok::LeftParenthesis));
                    let Some(Tok::Field(field)) = lexer_tokens.pop_front() else {
                        panic!();
                    };
                    assert_eq!(lexer_tokens.pop_front(), Some(Tok::RightParenthesis));
                    ret.push(MemoryExpr::Reg(field));
                }
                Tok::RegPlusImm => {
                    assert_eq!(lexer_tokens.pop_front(), Some(Tok::LeftParenthesis));
                    let Some(Tok::Field(field)) = lexer_tokens.pop_front() else {
                        panic!();
                    };
                    assert_eq!(lexer_tokens.pop_front(), Some(Tok::Comma));
                    let expr = parse_expr(&mut lexer_tokens);
                    assert_eq!(lexer_tokens.pop_front(), Some(Tok::RightParenthesis));
                    ret.push(MemoryExpr::RegPlusImm(field, expr));
                }
                Tok::RegPlusRegShifted => {
                    assert_eq!(lexer_tokens.pop_front(), Some(Tok::LeftParenthesis));
                    let Some(Tok::Field(field1)) = lexer_tokens.pop_front() else {
                        panic!();
                    };
                    assert_eq!(lexer_tokens.pop_front(), Some(Tok::Comma));
                    let Some(Tok::Field(field2)) = lexer_tokens.pop_front() else {
                        panic!();
                    };
                    assert_eq!(lexer_tokens.pop_front(), Some(Tok::Comma));
                    let expr = parse_expr(&mut lexer_tokens);
                    assert_eq!(lexer_tokens.pop_front(), Some(Tok::RightParenthesis));
                    ret.push(MemoryExpr::RegPlusRegShifted(field1, field2, expr));
                }
                other => panic!("{other:?}"),
            }
        }

        Self { exprs: ret }
    }

    pub fn to_rust(&self) -> proc_macro2::TokenStream {
        let mut accum = vec![];

        for expr in &self.exprs {
            match expr {
                MemoryExpr::AlignImm(i) => {
                    accum.push(quote! {
                        alignment = #i;
                    });
                }
                MemoryExpr::AlignShiftedImm(i, field) => {
                    let field = quote::format_ident!("{field}");
                    accum.push(quote! {
                        alignment = #i << u32::from(self.#field);
                    });
                }
                MemoryExpr::Reg(r) => {
                    let r = quote::format_ident!("{r}");
                    accum.push(quote! {
                        ops.push(MemoryOp::MovI {
                            rd: 0,
                            imm: memory_block & !(alignment - 1) as u64,
                        });
                        let base = u32::from(self.#r);
                        if base != 0 {
                            ops.push(MemoryOp::MovR {
                                rd: u32::from(self.#r),
                                rn: 0,
                            });
                            ops.push(MemoryOp::MovI {
                                rd: 0,
                                imm: 0,
                            });
                        }
                    });
                }
                MemoryExpr::RegPlusImm(r, i) => {
                    let r = quote::format_ident!("{r}");
                    let i = i.to_rust();
                    accum.push(quote! {
                        let base = u32::from(self.#r);
                        let imm = #i;

                        ops.push(MemoryOp::MovI {
                            rd: 0,
                            imm: memory_block & !(alignment - 1) as u64,
                        });

                        // Now r0 is the address we want to do the access to,
                        // so set the basereg by doing the inverse of the
                        // addressing mode calculation, ie base = r0 - imm
                        // We could do this more cleverly with a sub immediate.
                        if base != 0 {
                            ops.push(MemoryOp::MovI {
                                rd: base,
                                imm: imm as _,
                            });
                            ops.push(MemoryOp::Sub {
                                rd: base,
                                rn: 0,
                                rm: base,
                            });
                            // Clear r0 to avoid register compare mismatches
                            // when the memory block location differs between machines.
                            ops.push(MemoryOp::MovI {
                                rd: 0,
                                imm: 0,
                            });
                        } else {
                            // We borrow r1 as a temporary (not a problem
                            // as long as we don't leave anything in a register
                            // which depends on the location of the memory block)
                            ops.push(MemoryOp::MovI {
                                rd: 1,
                                imm: imm as _,
                            });
                            ops.push(MemoryOp::Sub {
                                rd: base,
                                rn: 0,
                                rm: 1,
                            });
                        }
                    });
                }
                MemoryExpr::RegPlusRegShifted(rd, rn, e) => {
                    let rd = quote::format_ident!("{rd}");
                    let rn = quote::format_ident!("{rn}");
                    let e = e.to_rust();
                    accum.push(quote! {
                        let base = u32::from(self.#rd);
                        let mut idx = u32::from(self.#rn);
                        let shift = #e;
                        if shift > 4 {
                            panic!("reg_plus_reg_shifted: bad shift size {shift}");
                        }

                        let mut saveidx = false;

                        if idx == 0 {
                            // save the index into some other register for the
                            // moment, because the risuop will trash r0
                            saveidx = true;
                            idx = 1;
                            if idx == base {
                                idx += 1;
                            }
                            ops.push(MemoryOp::MovR {
                                rd: idx,
                                rn: 0,
                            });
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
                            ops.push(MemoryOp::MovR {
                                rd: 0,
                                rn: idx,
                            });
                        } else if base != 0 {
                            ops.push(MemoryOp::MovI {
                                rd: 0,
                                imm: 0,
                            });
                        }
                    });
                }
            }
        }

        quote! { #(#accum)* }
    }
}

#[derive(Debug, PartialEq)]
enum Tok {
    Align,
    LeftParenthesis,
    RightParenthesis,
    Integer(u32),
    Reg,
    RegPlusImm,
    RegPlusRegShifted,
    Field(String),
    Comma,
    QuestionMark,
    Colon,
    Sextract,
    Multiplication,
    ShiftLeft,
    Equals,
}

#[test]
fn test_memory_parse() {
    assert_eq!(Memory::parse("align(1); reg($rn);").exprs, vec![
        MemoryExpr::AlignImm(1),
        MemoryExpr::Reg("rn".to_string()),
    ]);
    assert_eq!(Memory::parse("align(1 << $size); reg($rn);").exprs, vec![
        MemoryExpr::AlignShiftedImm(1, "size".to_string()),
        MemoryExpr::Reg("rn".to_string()),
    ]);
    assert_eq!(
        Memory::parse("align(1);reg_plus_imm($rn, $imm);").exprs,
        vec![
            MemoryExpr::AlignImm(1),
            MemoryExpr::RegPlusImm(
                "rn".to_string(),
                ArithmExpr::Terminal(ArithmExprTerminal::Field("imm".to_string()),)
            )
        ]
    );
    assert_eq!(
        Memory::parse(" align(8); reg_plus_imm($rn, $imm * 8); ").exprs,
        vec![
            MemoryExpr::AlignImm(8),
            MemoryExpr::RegPlusImm(
                "rn".to_string(),
                ArithmExpr::Multiplied(
                    ArithmExprTerminal::Field("imm".to_string()),
                    ArithmExprTerminal::Literal(8)
                )
            )
        ]
    );
    assert_eq!(
        Memory::parse("align(16); reg_plus_imm($rn, sextract($imm, 7) * 8);").exprs,
        vec![
            MemoryExpr::AlignImm(16),
            MemoryExpr::RegPlusImm(
                "rn".to_string(),
                ArithmExpr::Multiplied(
                    ArithmExprTerminal::Sextract("imm".to_string(), 7),
                    ArithmExprTerminal::Literal(8)
                )
            )
        ]
    );
    assert_eq!(
        Memory::parse("align(2); reg_plus_reg_shifted($rn, $rm, $shft ? 1 : 0);").exprs,
        vec![
            MemoryExpr::AlignImm(2),
            MemoryExpr::RegPlusRegShifted(
                "rn".to_string(),
                "rm".to_string(),
                ArithmExpr::Ternary(
                    Comparison::Not("shft".to_string(), 0),
                    Box::new(ArithmExpr::Terminal(ArithmExprTerminal::Literal(1))),
                    Box::new(ArithmExpr::Terminal(ArithmExprTerminal::Literal(0))),
                )
            )
        ]
    );
    assert_eq!(
        Memory::parse(" align(1); reg_plus_imm($rn, $idx == 1 ? 0 : sextract($imm, 9)); ").exprs,
        vec![
            MemoryExpr::AlignImm(1),
            MemoryExpr::RegPlusImm(
                "rn".to_string(),
                ArithmExpr::Ternary(
                    Comparison::Equals("idx".to_string(), 1),
                    Box::new(ArithmExpr::Terminal(ArithmExprTerminal::Literal(0))),
                    Box::new(ArithmExpr::Terminal(ArithmExprTerminal::Sextract(
                        "imm".to_string(),
                        9
                    )))
                )
            )
        ]
    );
    assert_eq!(
        Memory::parse(" align(8); reg_plus_imm($rn, sextract($imm, 9)); ").exprs,
        vec![
            MemoryExpr::AlignImm(8),
            MemoryExpr::RegPlusImm(
                "rn".to_string(),
                ArithmExpr::Terminal(ArithmExprTerminal::Sextract("imm".to_string(), 9))
            )
        ]
    );
    assert_eq!(
        Memory::parse(" align(8); reg_plus_imm($rn, $idx == 1 ? 0 : sextract($imm, 7) * 4); ")
            .exprs,
        vec![
            MemoryExpr::AlignImm(8),
            MemoryExpr::RegPlusImm(
                "rn".to_string(),
                ArithmExpr::Ternary(
                    Comparison::Equals("idx".to_string(), 1),
                    Box::new(ArithmExpr::Terminal(ArithmExprTerminal::Literal(0))),
                    Box::new(ArithmExpr::Multiplied(
                        ArithmExprTerminal::Sextract("imm".to_string(), 7),
                        ArithmExprTerminal::Literal(4)
                    ))
                )
            )
        ]
    );
}

#[test]
fn test_memory_codegen() {
    assert_eq!(
        Memory::parse("align(1); reg($rn);").to_rust().to_string(),
        quote! {
            alignment = 1u32;
            ops.push(MemoryOp::MovI {
                rd: 0,
                imm: memory_block & !(alignment - 1) as u64,
            });
            ops.push(MemoryOp::MovR {
                rd: u32::from(self.rn),
                rn: 0,
            });
            ops.push(MemoryOp::MovI { rd: 0, imm: 0, });
        }
        .to_string()
    );
    assert_eq!(
        Memory::parse(" align(1 << $sz); reg_plus_reg_shifted($rn, $rm, $shft ? 0 : 0); ")
            .to_rust()
            .to_string(),
        quote! {
            alignment = 1u32 << u32::from(self.sz);
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
                ops.push(MemoryOp::MovR { rd: idx, rn: 0, });
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
                ops.push(MemoryOp::MovR { rd: 0, rn: idx, });
            } else if base != 0 {
                ops.push(MemoryOp::MovI { rd: 0, imm: 0, });
            }
        }
        .to_string()
    );
    assert_eq!(
        Memory::parse("align(1); reg_plus_imm($rn, sextract($imm, 9));")
            .to_rust()
            .to_string(),
        quote! {
            alignment = 1u32;
            let base = u32::from(self.rn);
            let imm = sextract!(u32::from(self.imm), 9u32);
            ops.push(MemoryOp::MovI {
                rd: 0,
                imm: memory_block & !(alignment - 1) as u64,
            });
            if base != 0 {
                ops.push(MemoryOp::MovI { rd: base, imm: imm as _, });
                ops.push(MemoryOp::Sub {
                    rd: base,
                    rn: 0,
                    rm: base,
                });
                ops.push(MemoryOp::MovI { rd: 0, imm: 0, });
            } else {
                ops.push(MemoryOp::MovI { rd: 1, imm: imm as _, });
                ops.push(MemoryOp::Sub {
                    rd: base,
                    rn: 0,
                    rm: 1,
                });
            }
        }
        .to_string()
    );
}
