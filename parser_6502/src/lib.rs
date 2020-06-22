mod instructions;

pub use instructions::{Arg, ArgCode, Opcode, INSTRUCT_TABLE, Instruction, HexInstruction};
use logos::{Logos, Lexer, Span};
use std::convert::TryFrom;
use smallvec::{SmallVec};
use std::collections::HashMap;

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

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
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
    #[token(":")] Colon,
    #[token("=")] Equal,

    #[regex(r"[a-zA-Z]+")]
    Ident,

    #[token("\n")]
    Eoi,
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

    fn bad_num8(v: u16, ctx: &Context) -> ParserError {
        Self::new(&format!("bad number {}, needs to be less than 256", v), ctx)
    }

    fn unsupported_mode(i: &Instruction, ctx: &Context) -> ParserError {
        Self::new(&format!("addressing mode {:?} is not supported for this op", i.arg), ctx)
    }

    fn unrecognized_define(label: &str, ctx: &Context) -> ParserError {
        Self::new(&format!("no define found with name {}", label), ctx)
    }
}

type Context<'a> = Lexer<'a, Token>;

#[derive(Debug)]
pub struct Program {
    pub instructions: Vec<Instruction>,
    pub defines: HashMap<String, Arg>,
    line: usize,
}

impl Program {
    fn parse_num8(&self, v: u16, ctx: &Context) -> Result<u8, ParserError> {
        u8::try_from(v).map_err(|_| ParserError::bad_num8(v, ctx))
    }

    fn lookup_def(&self, name: &str, ctx: &Context) -> Result<&Arg, ParserError> {
        self.defines.get(name).ok_or_else(|| ParserError::unrecognized_define(name, ctx))
    }

    fn parse_define(&self, ctx: &mut Context) -> Result<Arg, ParserError> {
        if ctx.next() != Some(Token::Equal) {
            Err(ParserError::invalid_token(Token::Equal, ctx))
        } else {
            self.parse_arg(ctx)
        }
    }

    fn parse_arg(&self, ctx: &mut Context) -> Result<Arg, ParserError> {
        let mut args_span: SmallVec<[(Token, Span); 5]> = SmallVec::new();
        let mut args: SmallVec<[Token; 5]> = SmallVec::new();
        while let Some(t) = ctx.next() {
            if t == Token::Eoi {
                break;
            }
            args.push(t);
            args_span.push((t, ctx.span()));
        }

        match args[..] {
            // BRK
            [] => Ok(Arg::Implicit),
            // LDA #$12
            [Token::Immediate, Token::Num((v, NumType::Bit8))] => Ok(Arg::Immediate(self.parse_num8(v, ctx)?)),
            // LDA #label
            [Token::Immediate, Token::Ident] => {
                let def_span = args_span.last().unwrap().1.clone();
                Ok(*self.lookup_def(&ctx.source()[def_span], ctx)?)
            },
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
            [Token::OpenParen, Token::Num((v, _)), Token::CloseParen] => Ok(Arg::Indirect(v)),
            // LDA ($44,X)
            [Token::OpenParen, Token::Num((v, nt)), Token::Comma, Token::RegX, Token::CloseParen] if nt == NumType::Bit8 => Ok(Arg::IndexedIndirect(v as u8)),
            // LDA ($44),Y
            [Token::OpenParen, Token::Num((v, nt)), Token::CloseParen, Token::Comma, Token::RegY] if nt == NumType::Bit8 => Ok(Arg::IndirectIndexed(v as u8)),

            _ => Err(ParserError::invalid_token(Token::Immediate, ctx))
        }
    }

    fn next_instr(&mut self, ctx: &mut Context) -> Result<Option<Instruction>, ParserError> {
        match Lexer::next(ctx) {
            None => Ok(None),
            Some(Token::Ident) => {
                let op_name = str::to_uppercase(ctx.slice());
                let op = Opcode::from_string(&op_name);
                if let Some(op) = op {
                    Ok(Some(Instruction::new(op, self.parse_arg(ctx)?)))
                } else {
                    let define_name = ctx.slice();
                    let define = self.parse_define(ctx)?;
                    self.defines.insert(define_name.to_string(), define);
                    self.next_instr(ctx)
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
            defines: HashMap::new(),
            line: 0,
        };

        let mut lex: Lexer<'a, Token> = Token::lexer(txt);
        loop {
            let instr = p.next_instr(&mut lex)?;
            if instr.is_none() {
                break;
            }

            let i = instr.unwrap();
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
        for i in &self.instructions {
            match i.encode_as_hex() {
                None => continue,
                Some(HexInstruction::Arg0(o)) => hex.push(o),
                Some(HexInstruction::Arg1(o, a)) => {
                    hex.push(o);
                    hex.push(a);
                }
                Some(HexInstruction::Arg2(o, a1, a2)) => {
                    hex.push(o);
                    hex.push(a1);
                    hex.push(a2);
                }
            };
        }
        hex
    }
}
