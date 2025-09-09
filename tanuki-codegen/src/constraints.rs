// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright Manos Pitsidianakis

//! A shunting yard parser for Risu's C/Perl-like expressions
use proc_macro2::{Punct, Spacing};
use quote::{TokenStreamExt, quote};

#[derive(Debug)]
pub struct Constraint {
    pub original: String,
    tokens: Vec<Token>,
}

impl Constraint {
    pub fn parse(mut s: &str) -> Self {
        let original = s.to_string();

        let mut stack = vec![];
        let mut output = vec![];

        while !s.is_empty() {
            if s.chars().nth(0).unwrap().is_whitespace() {
                s = &s[1..];
                continue;
            }
            if s.as_bytes()[0].is_ascii_digit() {
                if s.starts_with("0x") {
                    const START: usize = "0x".len();
                    let mut i = START;
                    while !s[i..].is_empty()
                        && (s.as_bytes()[i].is_ascii_digit()
                            || (b'a'..=b'f').contains(&s.as_bytes()[i]))
                    {
                        i += 1;
                    }
                    if i == START {
                        panic!("Invalid hex expression: {s:?}");
                    }
                    output.push(Token::Integer(
                        u32::from_str_radix(&s[START..i], 16).expect("invalid hex literal"),
                    ));
                    s = &s[i..];
                } else {
                    let mut accum: u32 = s.as_bytes()[0] as u32 - '0' as u32;
                    s = &s[1..];
                    while !s.is_empty() && s.as_bytes()[0].is_ascii_digit() {
                        accum *= 10;
                        accum += s.as_bytes()[0] as u32 - '0' as u32;
                        s = &s[1..];
                    }
                    output.push(Token::Integer(accum));
                }
            } else if s.starts_with('$') {
                let mut i = 1;
                while !s[i..].is_empty() && s.as_bytes()[i].is_ascii_alphanumeric() {
                    i += 1;
                }
                output.push(Token::Bitfield(s[1..i].into()));
                s = &s[i..];
            } else {
                let op = Operator::parse(&mut s);
                match op {
                    Operator::LeftParenthesis => {
                        stack.push(op);
                        // output.push(Token::Operator(op));
                    }
                    Operator::RightParenthesis => {
                        while !matches!(stack.last(), Some(Operator::LeftParenthesis) | None) {
                            output.push(Token::Operator(stack.pop().unwrap()));
                        }
                        assert!(matches!(stack.pop().unwrap(), Operator::LeftParenthesis));
                        // output.push(Token::Operator(op));
                    }
                    op => {
                        while matches!(stack.last(), Some(other_op) if other_op.precedence() <= op.precedence())
                        {
                            output.push(Token::Operator(stack.pop().unwrap()));
                        }
                        stack.push(op);
                    }
                }
            }
        }
        while let Some(op) = stack.pop() {
            output.push(Token::Operator(op));
        }

        Self {
            original,
            tokens: output,
        }
    }

    pub fn to_rust(&self) -> proc_macro2::TokenStream {
        let mut stack = vec![];
        let mut accum = quote! {};

        for tok in &self.tokens {
            match tok {
                Token::Bitfield(f) => {
                    let f = if f == "type" {
                        quote::format_ident!("r#type")
                    } else {
                        quote::format_ident!("{f}")
                    };
                    stack.push(quote! { #f.as_u32() });
                }
                Token::Integer(i) => stack.push(quote! { #i }),
                Token::Operator(op) if op.left_associative() => {
                    let b = stack.pop();
                    let a = stack.pop();
                    stack.push(quote! { (#a #op #b) });
                }
                Token::Operator(op) => {
                    let a = stack.pop();
                    stack.push(quote! { (#op #a) });
                }
            }
        }
        while let Some(i) = stack.pop() {
            accum = quote! { #i #accum };
        }

        accum
    }
}

/// <https://en.cppreference.com/w/c/language/operator_precedence.html>
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Operator {
    LeftParenthesis,
    RightParenthesis,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Multiply,
    Divide,
    Add,
    Subtract,
    LogicalNot,
    #[allow(dead_code)]
    BitwiseNot,
    Equals,
    NotEquals,
    BitwiseAnd,
    LogicalAnd,
    LogicalOr,
}

impl quote::ToTokens for Operator {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::LogicalOr => {
                tokens.append(Punct::new('|', Spacing::Joint));
                tokens.append(Punct::new('|', Spacing::Alone));
            }
            Self::LogicalAnd => {
                tokens.append(Punct::new('&', Spacing::Joint));
                tokens.append(Punct::new('&', Spacing::Alone));
            }
            Self::BitwiseAnd => {
                tokens.append(Punct::new('&', Spacing::Alone));
            }
            Self::Equals => {
                tokens.append(Punct::new('=', Spacing::Joint));
                tokens.append(Punct::new('=', Spacing::Alone));
            }
            Self::NotEquals => {
                tokens.append(Punct::new('!', Spacing::Joint));
                tokens.append(Punct::new('=', Spacing::Alone));
            }
            Self::Less => {
                tokens.append(Punct::new('<', Spacing::Alone));
            }
            Self::LessEqual => {
                tokens.append(Punct::new('<', Spacing::Joint));
                tokens.append(Punct::new('=', Spacing::Alone));
            }
            Self::Greater => {
                tokens.append(Punct::new('>', Spacing::Alone));
            }
            Self::GreaterEqual => {
                tokens.append(Punct::new('>', Spacing::Joint));
                tokens.append(Punct::new('=', Spacing::Alone));
            }
            Self::LeftParenthesis | Self::RightParenthesis => {
                unreachable!()
            }
            Self::Multiply => {
                tokens.append(Punct::new('*', Spacing::Alone));
            }
            Self::Divide => {
                tokens.append(Punct::new('/', Spacing::Alone));
            }
            Self::Add => {
                tokens.append(Punct::new('+', Spacing::Alone));
            }
            Self::Subtract => {
                tokens.append(Punct::new('-', Spacing::Alone));
            }
            Self::BitwiseNot | Self::LogicalNot => {
                tokens.append(Punct::new('!', Spacing::Alone));
            }
        }
    }
}

impl Operator {
    fn parse(s: &mut &str) -> Self {
        let ret = match s.as_bytes()[0] {
            b'<' if s.starts_with("<=") => {
                *s = &s[2..];
                return Operator::LessEqual;
            }
            b'>' if s.starts_with(">=") => {
                *s = &s[2..];
                return Operator::GreaterEqual;
            }
            b'!' if s.starts_with("!=") => {
                *s = &s[2..];
                return Operator::NotEquals;
            }
            b'=' if s.starts_with("==") => {
                *s = &s[2..];
                return Operator::Equals;
            }
            b'&' if s.starts_with("&&") => {
                *s = &s[2..];
                return Operator::LogicalAnd;
            }
            b'|' if s.starts_with("||") => {
                *s = &s[2..];
                return Operator::LogicalOr;
            }
            b'(' => Operator::LeftParenthesis,
            b')' => Operator::RightParenthesis,
            b'<' => Operator::Less,
            b'>' => Operator::Greater,
            b'*' => Operator::Multiply,
            b'/' => Operator::Divide,
            b'+' => Operator::Add,
            b'-' => Operator::Subtract,
            b'!' => Operator::LogicalNot,
            b'&' => Operator::BitwiseAnd,
            b'|' => unimplemented!(),
            _ => unreachable!("{s:?}"),
        };
        *s = &s[1..];
        ret
    }

    pub const fn precedence(&self) -> u8 {
        match self {
            Self::LogicalOr => 12,
            Self::LogicalAnd => 11,
            Self::BitwiseAnd => 8,
            Self::Equals | Self::NotEquals => 7,
            Self::Less | Self::LessEqual | Self::Greater | Self::GreaterEqual => 6,
            Self::LeftParenthesis | Self::RightParenthesis => u8::MAX,
            Self::Multiply | Self::Divide => 3,
            Self::Add | Self::Subtract => 2,
            Self::LogicalNot | Self::BitwiseNot => 1,
        }
    }

    pub const fn left_associative(&self) -> bool {
        !matches!(
            self,
            Self::LeftParenthesis | Self::RightParenthesis | Self::LogicalNot | Self::BitwiseNot
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Operator(Operator),
    Integer(u32),
    Bitfield(String),
}

#[test]
fn test_constraint_parse() {
    use Token::*;

    use self::Operator::*;

    assert_eq!(Constraint::parse("$rn != 31 && $rn != $rt").tokens, vec![
        Bitfield("rn".into(),),
        Integer(31,),
        Operator(NotEquals,),
        Bitfield("rn".into(),),
        Bitfield("rt".into(),),
        Operator(NotEquals,),
        Operator(LogicalAnd,),
    ]);

    assert_eq!(
        Constraint::parse(" $rn != 31 && !($size & 0x01)").tokens,
        vec![
            Bitfield("rn".into()),
            Integer(31),
            Operator(NotEquals),
            Bitfield("size".into()),
            Integer(1),
            Operator(BitwiseAnd),
            Operator(LogicalNot),
            Operator(LogicalAnd)
        ]
    );
    assert_eq!(
        Constraint::parse("$rn != 31 && $rm != $rn && !($size == 3 && $Q == 0)").tokens,
        vec![
            Bitfield("rn".into()),
            Integer(31),
            Operator(NotEquals),
            Bitfield("rm".into()),
            Bitfield("rn".into()),
            Operator(NotEquals),
            Operator(LogicalAnd),
            Bitfield("size".into()),
            Integer(3),
            Operator(Equals),
            Bitfield("Q".into()),
            Integer(0),
            Operator(Equals),
            Operator(LogicalAnd),
            Operator(LogicalNot),
            Operator(LogicalAnd)
        ]
    );
    assert_eq!(
        Constraint::parse("$s < 2 || ($s == 2 && $Q == 1)").tokens,
        vec![
            Bitfield("s".into()),
            Integer(2),
            Operator(Less),
            Bitfield("s".into()),
            Integer(2),
            Operator(Equals),
            Bitfield("Q".into()),
            Integer(1),
            Operator(Equals),
            Operator(LogicalAnd),
            Operator(LogicalOr)
        ]
    );
    assert_eq!(
        Constraint::parse(
            "($op == 0 && $cm != 1 && $cm != 3 && $cm != 5 && $cm != 7 && $cm != 9 && $cm != 11 \
             && $cm != 15) || ($op == 1 && $cm == 14)"
        )
        .tokens,
        vec![
            Bitfield("op".into()),
            Integer(0),
            Operator(Equals),
            Bitfield("cm".into()),
            Integer(1),
            Operator(NotEquals),
            Operator(LogicalAnd),
            Bitfield("cm".into()),
            Integer(3),
            Operator(NotEquals),
            Operator(LogicalAnd),
            Bitfield("cm".into()),
            Integer(5),
            Operator(NotEquals),
            Operator(LogicalAnd),
            Bitfield("cm".into()),
            Integer(7),
            Operator(NotEquals),
            Operator(LogicalAnd),
            Bitfield("cm".into()),
            Integer(9),
            Operator(NotEquals),
            Operator(LogicalAnd),
            Bitfield("cm".into()),
            Integer(11),
            Operator(NotEquals),
            Operator(LogicalAnd),
            Bitfield("cm".into()),
            Integer(15),
            Operator(NotEquals),
            Operator(LogicalAnd),
            Bitfield("op".into()),
            Integer(1),
            Operator(Equals),
            Bitfield("cm".into()),
            Integer(14),
            Operator(Equals),
            Operator(LogicalAnd),
            Operator(LogicalOr),
        ]
    );
    assert_eq!(
        Constraint::parse(" ($Q == 0 && ($imm & 0x07)) || ($Q == 1 && (($imm & 0x0f) == 0x80))")
            .tokens,
        vec![
            Bitfield("Q".into()),
            Integer(0),
            Operator(Equals),
            Bitfield("imm".into()),
            Integer(0x07),
            Operator(BitwiseAnd),
            Operator(LogicalAnd),
            Bitfield("Q".into()),
            Integer(1),
            Operator(Equals),
            Bitfield("imm".into()),
            Integer(0x0f),
            Operator(BitwiseAnd),
            Integer(0x80),
            Operator(Equals),
            Operator(LogicalAnd),
            Operator(LogicalOr)
        ]
    );
}

#[test]
fn test_constraint_codegen() {
    assert_eq!(
        Constraint::parse("$rn != 31 && $rn != $rt")
            .to_rust()
            .to_string(),
        quote! {
            ((rn.as_u32() != 31u32) && (rn.as_u32() != rt.as_u32()))
        }
        .to_string()
    );
    assert_eq!(
        Constraint::parse(
            " ($Q == 0 && ($imm & 0x07)) || ($Q == 1 && (($imm & 0x0f) == 0x80))"
        ).to_rust().to_string(),
        quote! {
            (((Q.as_u32() == 0u32) && (imm.as_u32() & 7u32)) || ((Q.as_u32() == 1u32) && ((imm.as_u32() & 15u32) == 128u32)))
        }.to_string()
    );
    assert_eq!(
        Constraint::parse(" $rn != 31 && !($size & 0x01) ")
            .to_rust()
            .to_string(),
        quote! {
            ((rn . as_u32 () != 31u32) && (! (size . as_u32 () & 1u32)))
        }
        .to_string()
    );
}
