use num::Integer;
use enum_string::FromString;
use logos::{Logos, Lexer, Span};

// Note: callbacks can return `Option` or `Result`
fn hex_parse<T: Integer>(lex: &mut Lexer<Token>) -> Option<T> {
    let slice = lex.slice();
    let n = T::from_str_radix(&slice[1..], 16); // skip '$'

    n.ok()
}

#[derive(Logos, Debug, PartialEq)]
enum Token {
    #[error]
    #[regex(r"[ \t\f]+", logos::skip)]
    Error,

    #[regex(r";.*", logos::skip)]
    Comment,

    #[regex(r"\$[0-9a-fA-F]", hex_parse::<u8>)]
    #[regex(r"\$[0-9a-fA-F][0-9a-fA-F]", hex_parse::<u8>)]
    Num8(u8),

    #[regex(r"\$[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]", hex_parse::<u16>)]
    #[regex(r"\$[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]", hex_parse::<u16>)]
    Num16(u16),

    #[token("#")]
    Immediate,

    #[token("X")] RegX,
    #[token("Y")] RegY,
    #[token("A")] RegA,
    #[token("(")] OpenParen,
    #[token(")")] CloseParen,

    #[regex(r"[a-zA-Z]+")]
    Ident,

    #[token("\n")]
    Eoi,
}

#[derive(FromString, Clone, Copy, PartialEq, Debug)]
pub enum Opcode {
    END,
    NOP,
    LDA,
    LDX,
    LDY,
    STA,
    STX,
    STY,
}

#[derive(PartialEq, Debug)]
pub enum Arg {
    Implicit,
    Accumulator,
    Immediate(u8),
    Zeropage(u8),
    ZeropageX(u8),
    ZeropageY(u8),
    Relative(i8),
    Absolute(u16),
    AbsoluteX(u16),
    AbsoluteY(u16),
    Indirect(u16),
    IndexedIndirect(u8),
    IndirectIndexed(u8),
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
}

pub struct Program {
    pub instructions: Vec<Instruction>,
    line: usize,
}

#[derive(Debug)]
pub struct ParserError {
    line: usize,
    loc: Span,
    error: String,
}

impl ParserError {
    fn new(error: &str, line: usize, lexer: &Lexer<Token>) -> ParserError {
        ParserError {
            error: format!("Found error at {}:{} - {}\n\tContext token: {}",
                           line, lexer.span().start, error, lexer.slice()),
            line,
            loc: lexer.span(),
        }
    }

    fn invalid_token(exp: Token, line: usize, lexer: &Lexer<Token>) -> ParserError {
        Self::new(&format!("expected token of type {:?}", exp), line, lexer)
    }

    fn unrecognized_op(line: usize, lexer: &Lexer<Token>) -> ParserError {
        Self::new("unrecognized op code", line, lexer)
    }
}

impl Program {
    fn parse_imm(&self, lex: &mut Lexer<Token>) -> Result<Arg, ParserError> {
        let next = Lexer::next(lex);
        if let Some(t) = next {
            return match t {
                Token::Num8(v) => Ok(Arg::Immediate(v)),
                _ => Err(ParserError::invalid_token(Token::Num8(0), self.line, lex))
            };
        }
        Err(ParserError::invalid_token(Token::Num8(0), self.line, lex))
    }

    fn parse_arg(&self, lex: &mut Lexer<Token>) -> Result<Arg, ParserError> {
        let next = Lexer::next(lex);
        if let Some(t) = next {
            match t {
                Token::Eoi => Ok(Arg::Implicit),
                Token::Immediate => {
                    self.parse_imm(lex)
                },
                Token::RegA => Ok(Arg::Accumulator),
                Token::Num8(v) => Ok(Arg::Zeropage(v)),
                Token::Num16(v) => Ok(Arg::Absolute(v)),
                _ => Err(ParserError::invalid_token(Token::Immediate, self.line, lex))
            }
        } else {
            Ok(Arg::Implicit)
        }
    }

    fn next_instr(&self, lex: &mut Lexer<Token>) -> Result<Instruction, ParserError> {
        let op = Lexer::next(lex);
        if op.is_none() {
            return Ok(Instruction::end());
        }

        let op = op.unwrap();
        match op {
            Token::Ident => {
                let op_name = str::to_uppercase(lex.slice());
                let op = Opcode::from_string(&op_name);
                if let Some(op) = op {
                    Ok(Instruction::new(op, self.parse_arg(lex)?))
                } else {
                    Err(ParserError::unrecognized_op(self.line, lex))
                }
            }
            Token::Eoi => { self.next_instr(lex) }
            Token::Comment => { self.next_instr(lex) }
            _ => {
                Err(ParserError::invalid_token(Token::Ident, self.line, lex))
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
            p.instructions.push(i);
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
        assert!(Program::compile("LDA #$0001").is_err());
    }
}
