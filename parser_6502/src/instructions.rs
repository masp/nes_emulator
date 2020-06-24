use enum_string::FromString;
use lazy_static::lazy_static;

#[derive(FromString, Clone, Copy, PartialEq, Debug)]
pub enum Opcode {
    ADC,
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
}

#[derive(PartialEq, Debug, Copy, Clone)]
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

#[derive(Clone, Copy, PartialEq)]
pub enum ArgCode {
    IMP,
    IMM,
    ZPG,
    ZPX,
    ZPY,
    REL,
    ABS,
    ABX,
    ABY,
    IND,
    IXI,
    IDI,
}

impl ArgCode {
    pub fn num_bytes(&self) -> u8 {
        use ArgCode::*;
        match self {
            IMP |
            IMM => 0,
            ZPG |
            ZPX |
            ZPY |
            REL |
            IXI |
            IDI => 1,
            IND |
            ABS |
            ABX |
            ABY => 2,
        }
    }
}

impl Into<ArgCode> for Arg {
    fn into(self) -> ArgCode {
        use ArgCode::*;
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
type HexOpcode = u8;

static INVALID_OPCODE: HexOpcode = 0xFF;
const OPCODE_NUM_TABLE_SIZE: usize = 0xFE + 1;
// 0xFE is largest instruction opcode num
const OPCODE_TABLE_SIZE: usize = 1024; // 2^6 (opcode) * 2^4 (addressing mode, implicit/acc are same)

pub struct InstrTable {
    opcodenum_table: [HexOpcode; OPCODE_TABLE_SIZE],
    opcode_table: [(Opcode, ArgCode); OPCODE_NUM_TABLE_SIZE],
}

fn conv_idx(o: Opcode, a: ArgCode) -> u16 {
    let idx = (o as u16) | ((a as u16) << 6);
    assert!(idx < OPCODE_TABLE_SIZE as u16);
    idx
}

impl InstrTable {
    pub fn new() -> InstrTable {
        InstrTable {
            opcodenum_table: [INVALID_OPCODE; OPCODE_TABLE_SIZE],
            opcode_table: [(Opcode::NOP, ArgCode::IMM); OPCODE_NUM_TABLE_SIZE],
        }
    }

    fn update_table(&mut self, hexcode: HexOpcode, op: Opcode, arg: ArgCode) {
        let idx = usize::from(conv_idx(op, arg));
        assert_eq!(self.opcodenum_table[idx], 0xFF);
        self.opcodenum_table[idx] = hexcode;
        self.opcode_table[hexcode as usize] = (op, arg);
    }

    pub fn lookup(&self, o: Opcode, a: Arg) -> Option<HexOpcode> {
        let v = self.opcodenum_table[usize::from(conv_idx(o, a.into()))];
        if v != INVALID_OPCODE { Some(v) } else { None }
    }

    pub fn lookup_op(&self, code: &HexOpcode) -> Option<(Opcode, ArgCode)> {
        let (op, arg) = self.opcode_table.get(*code as usize)?;
        if *op == Opcode::NOP && *arg == ArgCode::IMM {
            None
        } else {
            Some((*op, *arg))
        }
    }
}

impl Default for InstrTable {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(PartialEq, Debug)]
pub struct Instruction {
    pub op: Opcode,
    pub arg: Arg,
}

pub enum HexInstruction {
    Arg0(HexOpcode),
    Arg1(HexOpcode, u8),
    Arg2(HexOpcode, u8, u8),
}

impl Instruction {
    pub fn from_bytes(bytes: &[u8]) -> Option<Instruction> {
        let hex_opcode = bytes.get(0)?;
        let (op, arg_code) = INSTRUCT_TABLE.lookup_op(hex_opcode)?;
        let b1 = bytes.get(1).copied();
        let b2 = bytes.get(2).copied();

        use ArgCode::*;
        use Arg::*;
        let arg = match arg_code {
            IMP => Implicit,
            IMM => Immediate(b1?),
            ZPG => Zeropage(b1?),
            ZPX => ZeropageX(b1?),
            ZPY => ZeropageY(b1?),
            REL => Relative(b1? as i8),
            IXI => IndexedIndirect(b1?),
            IDI => IndirectIndexed(b1?),
            IND => Indirect(u16::from_le_bytes([b1?, b2?])),
            ABS => Absolute(u16::from_le_bytes([b1?, b2?])),
            ABX => AbsoluteX(u16::from_le_bytes([b1?, b2?])),
            ABY => AbsoluteY(u16::from_le_bytes([b1?, b2?]))
        };
        Some(Instruction::new(op, arg))
    }

    pub fn invalid() -> Instruction {
        Instruction {
            op: Opcode::NOP,
            arg: Arg::Accumulator,
        }
    }

    pub fn new(op: Opcode, arg: Arg) -> Instruction {
        Instruction {
            op,
            arg,
        }
    }

    pub fn size_in_bytes(&self) -> usize {
        match self.encode_as_hex() {
            Some(HexInstruction::Arg0(_)) => 1,
            Some(HexInstruction::Arg1(_, _)) => 2,
            Some(HexInstruction::Arg2(_, _, _)) => 3,
            _ => 0,
        }
    }

    pub fn supports_mode(&self) -> bool {
        INSTRUCT_TABLE.lookup(self.op, self.arg).is_some()
    }

    pub fn encode_as_hex(&self) -> Option<HexInstruction> {
        let opcode = INSTRUCT_TABLE.lookup(self.op, self.arg)?;

        use Arg::*;
        Some(match self.arg {
            Implicit | Accumulator => HexInstruction::Arg0(opcode),
            Immediate(v) |
            Zeropage(v) |
            ZeropageX(v) |
            ZeropageY(v) |
            IndexedIndirect(v) |
            IndirectIndexed(v) => HexInstruction::Arg1(opcode, v),
            Relative(v) => HexInstruction::Arg1(opcode, v as u8),
            Absolute(v) |
            AbsoluteX(v) |
            AbsoluteY(v) |
            Indirect(v) => {
                let [a1, a2] = v.to_le_bytes();
                HexInstruction::Arg2(opcode, a1, a2)
            }
        })
    }
}

macro_rules! code {
    ($table:ident, $op:ident, $ar:ident, $cd:literal) => {
        $table.update_table($cd, Opcode::$op, ArgCode::$ar);
    };
}

lazy_static! {
    pub static ref INSTRUCT_TABLE: InstrTable = {
        let mut t = InstrTable::new();

        // ALU
        code!(t, ADC, IMM, 0x69);
        code!(t, ADC, ZPG, 0x65);
        code!(t, ADC, ZPX, 0x75);
        code!(t, ADC, ABS, 0x6D);
        code!(t, ADC, ABX, 0x7D);
        code!(t, ADC, ABY, 0x79);
        code!(t, ADC, IXI, 0x61);
        code!(t, ADC, IDI, 0x71);

        code!(t, SBC, IMM, 0xE9);
        code!(t, SBC, ZPG, 0xE5);
        code!(t, SBC, ZPX, 0xF5);
        code!(t, SBC, ABS, 0xED);
        code!(t, SBC, ABX, 0xFD);
        code!(t, SBC, ABY, 0xF9);
        code!(t, SBC, IXI, 0xE1);
        code!(t, SBC, IDI, 0xF1);

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

        code!(t, BPL, REL, 0x10);
        code!(t, BMI, REL, 0x30);
        code!(t, BVC, REL, 0x50);
        code!(t, BVS, REL, 0x70);
        code!(t, BCC, REL, 0x90);
        code!(t, BCS, REL, 0xB0);
        code!(t, BNE, REL, 0xD0);
        code!(t, BEQ, REL, 0xF0);

        code!(t, BRK, IMP, 0x00);

        code!(t, CMP, IMM, 0xC9);
        code!(t, CMP, ZPG, 0xC5);
        code!(t, CMP, ZPX, 0xD5);
        code!(t, CMP, ABS, 0xCD);
        code!(t, CMP, ABX, 0xDD);
        code!(t, CMP, ABY, 0xD9);
        code!(t, CMP, IXI, 0xC1);
        code!(t, CMP, IDI, 0xD1);

        code!(t, CPX, IMM, 0xE0);
        code!(t, CPX, ZPG, 0xE4);
        code!(t, CPX, ABS, 0xEC);

        code!(t, CPY, IMM, 0xC0);
        code!(t, CPY, ZPG, 0xC4);
        code!(t, CPY, ABS, 0xCC);

        code!(t, DEC, ZPG, 0xD6);
        code!(t, DEC, ZPX, 0xCE);
        code!(t, DEC, ABS, 0xDE);
        code!(t, DEC, ABX, 0xDD);

        code!(t, INC, ZPG, 0xE6);
        code!(t, INC, ZPX, 0xF6);
        code!(t, INC, ABS, 0xEE);
        code!(t, INC, ABX, 0xFE);

        code!(t, EOR, IMM, 0x49);
        code!(t, EOR, ZPG, 0x45);
        code!(t, EOR, ZPX, 0x55);
        code!(t, EOR, ABS, 0x4D);
        code!(t, EOR, ABX, 0x5D);
        code!(t, EOR, ABY, 0x59);
        code!(t, EOR, IXI, 0x41);
        code!(t, EOR, IDI, 0x51);

        // Flag Instructions
        code!(t, CLC, IMP, 0x18);
        code!(t, SEC, IMP, 0x38);
        code!(t, CLI, IMP, 0x58);
        code!(t, SEI, IMP, 0x78);
        code!(t, CLV, IMP, 0xB8);
        code!(t, CLD, IMP, 0xD8);
        code!(t, SED, IMP, 0xF8);

        code!(t, JMP, ABS, 0x4C);
        code!(t, JMP, IND, 0x6C);

        code!(t, JSR, ABS, 0x20);

        code!(t, LDA, IMM, 0xA9);
        code!(t, LDA, ZPG, 0xA5);
        code!(t, LDA, ZPX, 0xB5);
        code!(t, LDA, ABS, 0xAD);
        code!(t, LDA, ABX, 0xBD);
        code!(t, LDA, ABY, 0xB9);
        code!(t, LDA, IXI, 0xA1);
        code!(t, LDA, IDI, 0xB1);

        code!(t, LDX, IMM, 0xA2);
        code!(t, LDX, ZPG, 0xA6);
        code!(t, LDX, ZPY, 0xB6);
        code!(t, LDX, ABS, 0xAE);
        code!(t, LDX, ABY, 0xBE);

        code!(t, LDY, IMM, 0xA0);
        code!(t, LDY, ZPG, 0xA4);
        code!(t, LDY, ZPX, 0xB4);
        code!(t, LDY, ABS, 0xAC);
        code!(t, LDY, ABX, 0xBC);

        code!(t, LSR, IMM, 0x4A);
        code!(t, LSR, ZPG, 0x46);
        code!(t, LSR, ZPX, 0x56);
        code!(t, LSR, ABS, 0x4E);
        code!(t, LSR, ABX, 0x5E);

        code!(t, NOP, IMP, 0xEA);

        code!(t, ORA, IMM, 0x09);
        code!(t, ORA, ZPG, 0x05);
        code!(t, ORA, ZPX, 0x15);
        code!(t, ORA, ABS, 0x0D);
        code!(t, ORA, ABX, 0x1D);
        code!(t, ORA, ABY, 0x19);
        code!(t, ORA, IXI, 0x01);
        code!(t, ORA, IDI, 0x11);

        // Register Instructions
        code!(t, TAX, IMP, 0xAA);
        code!(t, TXA, IMP, 0x8A);
        code!(t, DEX, IMP, 0xCA);
        code!(t, INX, IMP, 0xE8);
        code!(t, TAY, IMP, 0xA8);
        code!(t, TYA, IMP, 0x98);
        code!(t, DEY, IMP, 0x88);
        code!(t, INY, IMP, 0xC8);

        // Bit shifting
        code!(t, ROL, IMP, 0x4A);
        code!(t, ROL, ZPG, 0x46);
        code!(t, ROL, ZPX, 0x56);
        code!(t, ROL, ABS, 0x4E);
        code!(t, ROL, ABX, 0x5E);

        code!(t, ROR, IMP, 0x6A);
        code!(t, ROR, ZPG, 0x66);
        code!(t, ROR, ZPX, 0x76);
        code!(t, ROR, ABS, 0x6E);
        code!(t, ROR, ABX, 0x7E);

        // Return
        code!(t, RTI, IMP, 0x40);
        code!(t, RTS, IMP, 0x60);

        // Store register
        code!(t, STA, ZPG, 0x85);
        code!(t, STA, ZPX, 0x95);
        code!(t, STA, ABS, 0x8D);
        code!(t, STA, ABX, 0x9D);
        code!(t, STA, ABY, 0x99);
        code!(t, STA, IXI, 0x81);
        code!(t, STA, IDI, 0x91);

        code!(t, STX, ZPG, 0x86);
        code!(t, STX, ZPY, 0x96);
        code!(t, STX, ABS, 0x8E);

        code!(t, STY, ZPG, 0x84);
        code!(t, STY, ZPX, 0x94);
        code!(t, STY, ABS, 0x8C);

        // Stack instructions
        code!(t, TXS, IMP, 0x9A);
        code!(t, TSX, IMP, 0xBA);
        code!(t, PHA, IMP, 0x48);
        code!(t, PLA, IMP, 0x68);
        code!(t, PHP, IMP, 0x08);
        code!(t, PLP, IMP, 0x28);

        t
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lookup_works() {
        assert_eq!(INSTRUCT_TABLE.lookup(Opcode::ADC, Arg::Absolute(10)), Some(0x6D));
        assert_eq!(INSTRUCT_TABLE.lookup(Opcode::ADC, Arg::Implicit), None);
    }
}
