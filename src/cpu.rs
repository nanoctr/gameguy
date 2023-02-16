use crate::{registers::Registers, mmu::Mmu};

struct Cpu {
    reg: Registers,
    pc: u16,
    sp: u16,
    mem: Mmu,
}

