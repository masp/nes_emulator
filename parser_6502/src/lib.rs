mod instructions;

use instructions::{Arg, Opcode};
use logos::{Logos, Lexer, Span};
use std::convert::TryFrom;
use smallvec::SmallVec;

fn hex_parse(lex: &mut Lexer<Token>) -> Option<u16> {
    u16::from_str_radix(&lex.slice()[1..], 16).ok() // skip '$'
}

fn dec_parse(lex: &mut Lexer<Token>) -> Option<u16> {
    u16::from_str_radix(&lex.slice(), 10).ok()
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
    Num(u16),

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

#[derive(PartialEq, Debug)]
pub struct Instruction {
    op: Opcode,
    arg: Arg,
}

impl Instruction {
    pub fn new(op: Opcode, arg: Arg) -> Instruction {
        Instruction {
            op,
            arg,
        }
    }

    pub fn end() -> Instruction {
        Instruction {
            op: Opcode::END,
            arg: Arg::Implicit,
        }
    }

    pub fn supports_mode(&self) -> bool {
        use Opcode::*;

        match self.op {
            NOP => self.arg == Arg::Implicit,
            _ => false
        }
    }
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
    ctx.source()[0..ctx.span().start].matches("\n").count()
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

        match &args[..] {
            &[] => Ok(Arg::Implicit),
            &[Token::Immediate, Token::Num(v)] => Ok(Arg::Immediate(self.parse_num8(v, ctx)?)),
            &[Token::RegA] => Ok(Arg::Accumulator),
            &[Token::Num(v)] if v < u8::MAX.into() => Ok(Arg::Zeropage(v as u8)),
            &[Token::Num(v)] => Ok(Arg::Absolute(v)),
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
