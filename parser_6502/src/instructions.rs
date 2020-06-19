use enum_string::FromString;

#[derive(FromString, Clone, Copy, PartialEq, Debug)]
pub enum Opcode {
    AND,
    ASL,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    JMP,
    JSR,
    LDA,
    LDX,
    LDY,
    LSR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    ROL,
    ROR,
    RTI,
    RTS,
    SBC,
    SEC,
    SED,
    SEI,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
    END = 64,
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

enum ArgHelper {
    IMP, IMM, ZPG, ZPX, ZPY, REL, ABS, ABX, ABY, IND, IXI, IDI
}

impl Into<ArgHelper> for Arg {
    fn into(self) -> ArgHelper {
        use ArgHelper::*;
        use Arg::*;

        match self {
            Implicit | Accumulator => IMP,
            Immediate(_) => IMM,
            Zeropage(_) => ZPG,
            ZeropageX(_) => ZPX,
            ZeropageY(_) => ZPY,
            Relative(_) => REL,
            Absolute(_) => ABS,
            AbsoluteX(_) => ABX,
            AbsoluteY(_) => ABY,
            Indirect(_) => IND,
            IndexedIndirect(_) => IXI,
            IndirectIndexed(_) => IDI,
        }
    }
}

// We use a fixed lookup table where each combination of instruction + addressing mode
// has a unique address in the table that matches to the 16 bit value
type OpcodeNum = u16;
static INVALID_OPCODE: OpcodeNum = 0xFF;

struct InstrTable {
    opcodenum_table: [OpcodeNum; 512], // 2^6 (opcode) * 2^3 (addressing mode, implicit/acc are same)
}

fn conv_idx(o: Opcode, a: ArgHelper) -> usize {
    let mut idx: usize = 0;
    idx = o as usize;
    idx |= (a.into() as usize) << 6;
    idx
}

impl InstrTable {
    pub fn lookup(&self, o: Opcode, a: Arg) -> OpcodeNum {
        self.opcodenum_table[conv_idx(o, a.into())]
    }
}

macro_rules! code {
    ($table:ident, $op:ident, $ar:ident, $cd:literal) => {
        $table.opcodenum_table[InstrTable::conv_idx(Opcode::$op, ArgHelper::$ar)] = $cd;
    };
}

lazy_static! {
    static ref INSTRUCT_TABLE: InstrTable = {
        let mut t = InstrTable::new();

        code!(t, ADC, IMM, 0x69);
        code!(t, ADC, ZPG, 0x65);
        code!(t, ADC, ZPX, 0x75);
        code!(t, ADC, ABS, 0x6D);
        code!(t, ADC, ABX, 0x7D);
        code!(t, ADC, ABY, 0x79);
        code!(t, ADC, IXI, 0x61);
        code!(t, ADC, IDI, 0x71);

        code!(t, AND, IMM, 0x29);
        code!(t, AND, ZPG, 0x25);
        code!(t, AND, ZPX, 0x35);
        code!(t, AND, ABS, 0x2D);
        code!(t, AND, ABX, 0x3D);
        code!(t, AND, ABY, 0x39);
        code!(t, AND, IXI, 0x21);
        code!(t, AND, IDI, 0x31);

        code!(t, ASL, IMM, 0x0A);
        code!(t, ASL, ZPG, 0x06);
        code!(t, ASL, ZPX, 0x16);
        code!(t, ASL, ABS, 0x0E);
        code!(t, ASL, ABX, 0x1E);

        code!(t, BCC, REL, 0x90);
        code!(t, BCS, REL, 0xB0);
        code!(t, BEQ, REL, 0xF0);
        code!(t, BCC, REL, 0x90);
        code!(t, BCC, REL, 0x90);
        code!(t, BCC, REL, 0x90);

        code!(t, BRK, IMP, 0x00);
        code!(t, NOP, IMP, 0xEA);

        t
    };
}
