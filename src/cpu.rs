use program::Program;

pub struct CpuFlags {
    c: bool,
    z: bool,
    i: bool,
    d: bool,
    b: bool,
    v: bool,
    n: bool
}

pub struct Cpu {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub pc: u16,
    pub sp: u8,
    status: CpuFlags,

    mem: Vec<u8>
}

enum Instruction {
    LDA_I(u8),
    LDX_I(u8),
    LDY_I(u8),
}

impl Cpu {
    pub fn new(mem_size: u16) -> Cpu {
        Cpu {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0,
            status: CpuFlags {
                c: false,
                z: false,
                i: false,
                d: false,
                b: false,
                v: false,
                n: false
            },
            mem: Vec::with_capacity(mem_size as usize)
        }
    }

    pub fn load_program(prog: Program) {

    }

    fn fetch(&mut self) -> Instruction {
        Instruction::LDA_I(5)
    }

    pub fn tick(&mut self) {
        let i = self.fetch();
        use Instruction::*;
        match i {
            LDA_I(i) => { self.a = i; }
            LDX_I(i) => { self.x = i; }
            LDY_I(i) => { self.y = i; }
        }
    }
}