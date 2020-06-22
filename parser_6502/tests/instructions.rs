use parser_6502::*;
use claim::*;

macro_rules! assert_compiles {
    ($prog:literal, $opcode:ident, $argtype:ident) => {
        let p = Program::compile($prog);
        assert_ok!(&p);
        assert_eq!(p.unwrap().instructions,
            &[Instruction::new(Opcode::$opcode, Arg::$argtype)]);
    };

    ($prog:literal, $opcode:ident, $argtype:ident, $arg:literal) => {
        let p = Program::compile($prog);
        assert_ok!(&p);
        assert_eq!(p.unwrap().instructions,
            &[Instruction::new(Opcode::$opcode, Arg::$argtype($arg))]);
    };
}

macro_rules! assert_fails {
    ($prog:literal) => {
        assert_err!(Program::compile($prog));
    }
}

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

#[test]
fn ignores_comments() {
    assert!(Program::compile("; My comment √sqrt\n   LDA #2; another comment").is_ok());
}

#[test]
fn reads_defines() {
    let p = Program::compile(concat!(
    "myvar = $12\n",
    "lda #myvar",
    ));

    assert_ok!(&p);
    assert_eq!(&p.unwrap().instructions, &[
        Instruction::new(Opcode::LDA, Arg::Zeropage(18))
    ]);
}

#[test]
fn nop_meets_spec() {
    assert_compiles!("NOP", NOP, Implicit);
    assert_fails!("NOP #$1");
    assert_fails!("NOP $1");
}

#[test]
fn lda_meets_spec() {
    assert_compiles!("LDA #$44", LDA, Immediate, 0x44);
    assert_compiles!("LDA $44", LDA, Zeropage, 0x44);
    assert_compiles!("LDA $44,X", LDA, ZeropageX, 0x44);
    assert_fails!("LDA $44,Y");
    assert_compiles!("LDA $4400", LDA, Absolute, 0x4400);
    assert_compiles!("LDA $4400,X", LDA, AbsoluteX, 0x4400);
    assert_compiles!("LDA $4400,Y", LDA, AbsoluteY, 0x4400);
    assert_compiles!("LDA ($44,X)", LDA, IndexedIndirect, 0x44);
    assert_fails!("LDA ($4400),X");
    assert_fails!("LDA ($44,Y)");

    assert_compiles!("LDA ($44),Y", LDA, IndirectIndexed, 0x44);
    assert_fails!("LDA ($44),X");
    assert_fails!("LDA ($4400),Y");
}

fn compile(txt: &str) -> Vec<u8> {
    Program::compile(txt).unwrap().dump_hex()
}

#[test]
fn dump_hex() {
    assert_eq!(compile("LDA #2"), [0xA9, 0x02]);
    assert_eq!(compile("JMP ($00f0)"), [0x6C, 0xF0, 0x00]);
}