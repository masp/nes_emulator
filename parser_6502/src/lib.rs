mod instructions;

pub use instructions::{Arg, ArgCode, Opcode, INSTRUCT_TABLE};
use logos::{Logos, Lexer, Span};
use std::convert::TryFrom;
use smallvec::{SmallVec};
use crate::instructions::{Instruction, HexInstruction};

#[derive(Debug, PartialEq, Copy, Clone)]
enum NumType {
    Bit8,
    Bit16,
}

fn hex_parse(lex: &mut Lexer<Token>) -> Option<(u16, NumType)> {
    let num_type = if lex.slice().len() > 3 {
        NumType::Bit16
    } else {
        NumType::Bit8
    };
    u16::from_str_radix(&lex.slice()[1..], 16).map(|x| (x, num_type)).ok() // skip '$'
}

fn dec_parse(lex: &mut Lexer<Token>) -> Option<(u16, NumType)> {
    u16::from_str_radix(&lex.slice(), 10).map(|x| {
        let nt = if x < u8::MAX.into() { NumType::Bit8 } else { NumType::Bit16 };
        (x, nt)
    }).ok()
}

#[derive(Logos, Debug, PartialEq)]
enum Token {
    #[error]
    #[regex(r"[ \t\f]+", logos::skip)]
    Error,

    #[regex(r";[^\n]*", logos::skip)]
    Comment,

    #[regex(r"\$[0-9a-fA-F]+", hex_parse)]
    #[regex(r"[0-9]+", dec_parse)]
    Num((u16, NumType)),

    #[token("#")]
    Immediate,

    #[token("X")] RegX,
    #[token("Y")] RegY,
    #[token("A")] RegA,
    #[token("(")] OpenParen,
    #[token(")")] CloseParen,
    #[token(",")] Comma,

    #[regex(r"[a-zA-Z]+")]
    Ident,

    #[token("\n")]
    Eoi,
}

#[derive(Debug)]
pub struct Program {
    pub instructions: Vec<Instruction>,
    line: usize,
}

#[derive(Debug)]
pub struct ParserError {
    loc: Span,
    error: String,
}

fn count_lines(ctx: &Context) -> usize {
    ctx.source()[0..ctx.span().start].matches('\n').count()
}

impl ParserError {
    fn new(error: &str, ctx: &Context) -> ParserError {
        let line = count_lines(ctx);
        ParserError {
            error: format!("Found error at line {} - {}\n\tContext token: {}",
                           line, error, ctx.slice()),
            loc: ctx.span(),
        }
    }

    fn invalid_token(exp: Token, ctx: &Context) -> ParserError {
        Self::new(&format!("expected token of type {:?}", exp), ctx)
    }

    fn unrecognized_op(ctx: &Context) -> ParserError {
        Self::new("unrecognized op code", ctx)
    }

    fn bad_num8(v: u16, ctx: &Context) -> ParserError {
        Self::new(&format!("bad number {}, needs to be less than 256", v), ctx)
    }

    fn unsupported_mode(i: &Instruction, ctx: &Context) -> ParserError {
        Self::new(&format!("addressing mode {:?} is not supported for this op", i.arg), ctx)
    }
}

type Context<'a> = Lexer<'a, Token>;

impl Program {
    fn parse_num8(&self, v: u16, ctx: &Context) -> Result<u8, ParserError> {
        u8::try_from(v).map_err(|_| ParserError::bad_num8(v, ctx))
    }

    fn parse_arg(&self, ctx: &mut Context) -> Result<Arg, ParserError> {
        let args: SmallVec<[Token; 8]> = ctx.take_while(|t| t != &Token::Eoi).collect();

        match args[..] {
            // BRK
            [] => Ok(Arg::Implicit),
            // LDA #$12
            [Token::Immediate, Token::Num((v, NumType::Bit8))] => Ok(Arg::Immediate(self.parse_num8(v, ctx)?)),
            // ROR A
            [Token::RegA] => Ok(Arg::Accumulator),

            // LDA $44
            [Token::Num((v, nt))] if nt == NumType::Bit8 => Ok(Arg::Zeropage(v as u8)),
            // LDA $44,X
            [Token::Num((v, nt)), Token::Comma, Token::RegX] if nt == NumType::Bit8 => Ok(Arg::ZeropageX(v as u8)),
            // LDA $44,Y
            [Token::Num((v, nt)), Token::Comma, Token::RegY] if nt == NumType::Bit8 => Ok(Arg::ZeropageY(v as u8)),

            // LDA $4400
            [Token::Num((v, nt))] if nt == NumType::Bit16 => Ok(Arg::Absolute(v)),
            // LDA $4400,X
            [Token::Num((v, nt)), Token::Comma, Token::RegX] if nt == NumType::Bit16 => Ok(Arg::AbsoluteX(v)),
            // LDA $4400,Y
            [Token::Num((v, nt)), Token::Comma, Token::RegY] if nt == NumType::Bit16 => Ok(Arg::AbsoluteY(v)),

            // JMP ($3000)
            [Token::OpenParen, Token::Num((v, nt)), Token::CloseParen] => Ok(Arg::Indirect(v)),
            // LDA ($44,X)
            [Token::OpenParen, Token::Num((v, nt)), Token::Comma, Token::RegX, Token::CloseParen] if nt == NumType::Bit8 => Ok(Arg::IndexedIndirect(v as u8)),
            // LDA ($44),Y
            [Token::OpenParen, Token::Num((v, nt)), Token::CloseParen, Token::Comma, Token::RegY] if nt == NumType::Bit8 => Ok(Arg::IndirectIndexed(v as u8)),

            _ => Err(ParserError::invalid_token(Token::Immediate, ctx))
        }
    }

    fn next_instr(&self, ctx: &mut Context) -> Result<Instruction, ParserError> {
        match Lexer::next(ctx) {
            None => Ok(Instruction::end()),
            Some(Token::Ident) => {
                let op_name = str::to_uppercase(ctx.slice());
                let op = Opcode::from_string(&op_name);
                if let Some(op) = op {
                    Ok(Instruction::new(op, self.parse_arg(ctx)?))
                } else {
                    Err(ParserError::unrecognized_op(ctx))
                }
            }
            Some(Token::Eoi) => { self.next_instr(ctx) }
            _ => {
                Err(ParserError::invalid_token(Token::Ident, ctx))
            }
        }
    }

    pub fn compile<'a>(txt: &'a str) -> Result<Program, ParserError> {
        let mut p = Program {
            instructions: Vec::new(),
            line: 0,
        };

        let mut lex: Lexer<'a, Token> = Token::lexer(txt);
        loop {
            let i = p.next_instr(&mut lex)?;
            if i.op == Opcode::END {
                break;
            }

            if i.supports_mode() {
                p.instructions.push(i);
            } else {
                return Err(ParserError::unsupported_mode(&i, &lex));
            }
        }

        Ok(p)
    }

    pub fn dump_hex(&self) -> Vec<u8> {
        let mut hex = Vec::new();
        for i in self.instructions {
            match i.encode_as_hex() {
                None => continue,
                Some(HexInstruction::Arg0(o)) => hex.push(o),
                Some(HexInstruction::Arg1(o, a)) => {
                    hex.push(o);
                    hex.push(a);
                }
                Some(HexInstruction::Arg2(o, a)) => {
                    let [a1, a2] = a.to_be_bytes();
                    hex.push(o);
                    hex.push(a2);
                    hex.push(a2);
                }
            };
        }
        hex
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_simple_program() {
        let p = Program::compile(concat!(
        "LDX #$11\n",
        "LDA $5"
        )).unwrap();

        assert_eq!(p.instructions, [
            Instruction { op: Opcode::LDX, arg: Arg::Immediate(17) },
            Instruction { op: Opcode::LDA, arg: Arg::Zeropage(5) }
        ]);
    }

    #[test]
    fn prevents_two_byte_immediates() {
        assert!(Program::compile("LDA #$100").is_err());
    }

    #[test]
    fn parses_decimal_numbers() {
        assert_eq!(
            Program::compile("LDA #2").unwrap().instructions,
            [Instruction::new(Opcode::LDA, Arg::Immediate(2))]
        );
    }
}
