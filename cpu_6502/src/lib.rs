use parser_6502::{Instruction, Program, Opcode, Arg};
use std::ops::Range;

#[derive(Debug, Clone)]
pub struct CpuFlags {
    c: bool,
    z: bool,
    i: bool,
    d: bool,
    b: bool,
    v: bool,
    n: bool,
}

impl CpuFlags {
    fn is_zero(v: &u8) -> bool { v == 0 }
    fn is_negative(v: &u8) -> bool { (v & (1 << 7)) != 0 }

    fn update_nz(&self, v: &u8) -> CpuFlags {
        let mut n = self.clone();
        n.z = is_zero(v);
        n.n = is_negative(v);
        n
    }
}

type Addr = u16;

#[derive(Debug)]
struct Mem {
    mem: Vec<u8>,
}

impl Mem {
    pub fn new(sz: u16) -> Mem {
        Mem {
            mem: vec![0; sz as usize],
        }
    }

    pub fn fetch_u8(&self, addr: Addr) -> u8 {
        self.mem[addr as usize]
    }

    pub fn fetch_u16(&self, addr: Addr) -> u16 {
        u16::from_le_bytes([self.mem[addr as usize], self.mem[(addr + 1) as usize]])
    }

    pub fn mut_u8(&mut self, addr: Addr, v: u8) -> Option<&mut u8> {
        self.mem.get_mut(addr as usize)
    }

    pub fn store_u16(&mut self, addr: Addr, v: u16) {
        let [v1, v2] = v.to_le_bytes();
        self.store_u8(addr, v1);
        self.store_u8(addr + 1, v2);
    }

    pub fn idx(&self, r: Range<Addr>) -> &[u8] {
        &self.mem[r.start as usize..r.end as usize]
    }

    pub fn idx_mut(&mut self, r: Range<Addr>) -> &mut [u8] {
        &mut self.mem[r.start as usize..r.end as usize]
    }
}


#[derive(Debug)]
pub struct Cpu {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub pc: Addr,
    pub sp: u8,
    flags: CpuFlags,
    mem: Mem,
    prog_code: Option<Range<u16>>,
}

impl Cpu {
    pub fn new(mem_size: u16) -> Cpu {
        Cpu {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0,
            flags: CpuFlags {
                c: false,
                z: false,
                i: false,
                d: false,
                b: false,
                v: false,
                n: false,
            },
            mem: Mem::new(mem_size),
            prog_code: None,
        }
    }

    pub fn load_program(&mut self, program: &Program, start: Addr) {
        let prog_data = program.dump_hex();
        let r = start..start + prog_data.len() as u16;
        self.prog_code = Some(r.clone());
        self.mem.idx_mut(r).copy_from_slice(&prog_data);
        self.pc = start;
    }

    fn fetch(&mut self) -> Option<Instruction> {
        let next = &self.mem.idx(self.pc..self.prog_code.as_ref()?.end);
        let ret = Instruction::from_bytes(next);
        if let Some(i) = ret.as_ref() {
            self.pc += i.size_in_bytes() as u16;
        }
        ret
    }

    fn add_with_carry(&mut self, v1: u8, v2: u8) -> u8 {
        let (new_val, did_overflow) = v1.overflow_add(v2);
        if did_overflow {
            self.flags.c = true;
        }
        new_val
    }

    fn sub_with_carry(&mut self, v1: u8, v2: u8) -> u8 {
        let (new_val, did_overflow) = v1.overflow_sub(v2);
        if did_overflow {
            self.flags.c = true;
        }
        new_val
    }

    fn resolve_addr(&self, arg: &Arg) -> Addr {
        match arg {
            Arg::Zeropage(a) => *a as Addr,
            Arg::ZeropageX(a) => a.wrapping_add(self.x) as Addr,
            Arg::ZeropageY(a) => a.wrapping_add(self.y) as Addr,
            Arg::Indirect(a) => self.mem.fetch_u16(*a),
            Arg::IndexedIndirect(a) => self.mem.fetch_u16(a.wrapping_add(self.x)),
            Arg::IndirectIndexed(a) => self.mem.fetch_u16(*a as u16) + self.y as u16,
            Arg::Relative(a) => self.pc + a as Addr,
            Arg::Absolute(a) => a,
            Arg::AbsoluteX(a) => a + self.x as u16,
            Arg::AbsoluteY(a) => a + self.y as u16,
            Arg::Immediate(v) |
            Arg::Implicit |
            Arg::Accumulator => panic!(format!("invalid arg type {:?} for address", a)),
        }
    }

    fn resolve_val(&self, a: &Arg) -> u8 {
        match a {
            Arg::Immediate(v) => *v,
            _ => self.mem.fetch_u8(self.resolve_addr(a))
        }
    }

    pub fn tick(&mut self) -> Option<Instruction> {
        let i = self.fetch()?;

        let Instruction { op: o, arg: a } = i;
        let new_flags = match o {
            Opcode::TAX => self.assign_reg(self.a, &mut self.x),
            Opcode::TXA => self.assign_reg(self.x, &mut self.a),
            Opcode::TAY => self.assign_reg(self.a, &mut self.y),
            Opcode::TYA => self.assign_reg(self.y, &mut self.a),

            Opcode::INC => self.add_reg(&mut self.a, 1),
            Opcode::DEC => { self.a = self.a.wrapping_sub(1); self.flags.update_nz(&self.a) }
            Opcode::INX => self.x = self.x.wrapping_add(1),
            Opcode::DEX => self.x = self.x.wrapping_sub(1),
            Opcode::INY => self.y = self.y.wrapping_add(1),
            Opcode::DEY => self.y = self.y.wrapping_sub(1),

            Opcode::LDA => self.a = self.resolve_val(&a),
            Opcode::LDX => self.x = self.resolve_val(&a),
            Opcode::LDY => self.y = self.resolve_val(&a),
            Opcode::STA => self.mem.store_u8(self.resolve_addr(&a), self.a),
            Opcode::STX => self.mem.store_u8(self.resolve_addr(&a), self.x),
            Opcode::STY => self.mem.store_u8(self.resolve_addr(&a), self.y),

            Opcode::ADC => {
                self.a + self.resolve_val(&a)
            }

            _ => unimplemented!("opcode not supported yet!")
        };
        self.flags = new_flags;

        Some(i)
    }

    fn assign_reg(&self, src: u8, dst: &mut u8) -> CpuFlags {
        *dst = src;
        self.flags.update_nz(dst)
    }

    fn add_reg(&self, dst: &mut u8, amt: u8) -> CpuFlags {
        let amt = amt + if self.status.c { 1 } else { 0 };
        let (new_val, did_overflow) = dst.overflow_add(amt);
        let mut flags = self.status.update_nz(new_val);

        if did_overflow && new_val > 1 {
            // For INC instructions, we don't set carry bit
            flags.c = true;
        }

        flags
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn loads_simple_registers() {
        let mut cpu = Cpu::new(16);
        let p = Program::compile(concat!(
        "LDA #$05\n",
        "TAX"
        )).unwrap();

        cpu.load_program(&p, 8);
        assert_eq!(cpu.tick(), Some(Instruction::new(Opcode::LDA, Arg::Immediate(0x05))));
        assert_eq!(cpu.a, 5);
        assert_eq!(cpu.tick(), Some(Instruction::new(Opcode::TAX, Arg::Implicit)));
        assert_eq!(cpu.x, 5);
    }
}