use logos::{Logos, Lexer, Span};
use crate::Arg::Immediate;

// Note: callbacks can return `Option` or `Result`
fn hex_parse(lex: &mut Lexer<Token>) -> Option<u16> {
    let slice = lex.slice();
    let n = u16::from_str_radix(&slice[1..], 16); // skip '$'

    n.ok()
}

#[derive(Logos, Debug, PartialEq)]
enum Token {
    #[error]
    #[regex(r"[ \t\f]+", logos::skip)]
    Error,

    #[regex(r";.*", logos::skip)]
    Comment,

    #[regex(r"\$[0-9a-fA-F]+", hex_parse)]
    Number(u16),

    #[token("#")]
    Immediate,

    #[token("X")] RegX,
    #[token("Y")] RegY,
    #[token("A")] RegA,

    #[regex(r"[a-zA-Z]+")]
    Ident,

    #[token("\n")]
    Eoi,
}

#[derive(PartialEq, Debug)]
enum Opcode {
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
enum Arg {
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

pub struct ParseError {
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
            loc,
        }
    }

    fn invalid_token(exp: Token, line: usize, lexer: &Lexer<Token>) -> ParserError {
        ParserError::new(format!("expected token of type {}", exp), line, lexer)
    }

    fn unrecognized_op(line: usize, lexer: &Lexer<Token>) -> ParserError {
        ParserError::new("unrecognized op code", line, lexer)
    }
}

impl Program {
    fn next_instr(&mut self, lex: &mut Lexer<Token>) -> Result<Instruction, ParserError> {
        let op = lex.next();
        if op.is_none() {
            return Ok(Instruction::end());
        }

        let op = op.unwrap();
        match op {
            Token::Ident => {
                let op_name = lex.slice();
                match op_name {
                    "ADC" => Ok(Instruction::end()),
                    _ => Err(ParserError::unrecognized_op(self.line, lex))
                }
            }
            Token::Eoi => { self.next_instr(lex) }
            Token::Comment => { self.next_instr(lex) }
            _ => {
                Err(ParserError::invalid_token(Token::Ident, self.line, lex))
            }
        }
    }

    pub fn compile(txt: &str) -> Result<Program, String> {
        let mut p = Program {
            instructions: Vec::new(),
        };

        let mut lex = Token::lexer(txt);
        while let Some(t) = lex.next() {
            println!(";
                { : ? }
                ", t);
        }

        p.instructions.push(Instruction {
            op: Opcode::LDA,
            arg: Immediate(10),
        });
        Ok(p)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let p = Program::compile(concat!(
        "ADC # $11\n
                ",
        "LDA $ 5
                "
        )).unwrap();

        assert_eq!(p.instructions, [
            Instruction { op: Opcode::LDA, arg: Arg::Immediate(17) },
            Instruction { op: Opcode::LDA, arg: Arg::Zeropage(5) }
        ]);
    }
}
