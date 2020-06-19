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