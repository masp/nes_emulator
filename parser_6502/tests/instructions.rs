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
    assert_fails!("DA $44,Y");
    assert_compiles!("LDA $4400", LDA, Absolute, 0x4400);
    assert_compiles!("LDA #$44", LDA, Immediate, 0x44);
    assert_compiles!("LDA #$44", LDA, Immediate, 0x44);
    assert_compiles!("LDA #$44", LDA, Immediate, 0x44);

    Immediate     LDA #$44      $A9  2   2
    Zero Page     LDA $44       $A5  2   3
    Zero Page,X   LDA $44,X     $B5  2   4
    Absolute      LDA $4400     $AD  3   4
    Absolute,X    LDA $4400,X   $BD  3   4+
    Absolute,Y    LDA $4400,Y   $B9  3   4+
    Indirect,X    LDA ($44,X)   $A1  2   6
    Indirect,Y    LDA ($44),Y   $B1  2   5+
}

#[test]
fn adc_meets_spec() {
    assert_compiles!("ADC #$12", LDA, Immediate, 18);
}