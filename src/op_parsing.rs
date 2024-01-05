mod helper;

use crate::{
    instruction::{
        ArithmeticSource, Condition, IncDecOp, IncdecDestination, IncdecLongDestination,
        Instruction, IoMemoryOffset, LoadDestination, LoadLongDestination, LoadLongSource,
        LoadSource,
    },
    mmu::Mmu,
    op_parsing::helper::RegOrMemHl,
    registers::{LongRegister, Register},
};

pub fn parse_next_instr(mem: &Mmu, mut pc: u16) -> Instruction {
    use Instruction::*;
    use LoadDestination as LdDst;
    use LoadLongDestination as LdLongDest;
    use LoadLongSource as LdLongSrc;
    use LoadSource as LdSrc;
    use LongRegister::*;
    use Register::*;

    let opcode = mem.read(pc);
    pc += 1;

    match opcode {
        0xD9 => unimplemented!("RETI"),
        0x27 => unimplemented!("DAA"),

        0x00 => NOP,
        0x10 => STOP,
        0x76 => HALT,

        // 8bit loads
        0x02 => LD(LdDst::MemoryAtRegister(BC), LdSrc::Register(A)),
        0x12 => LD(LdDst::MemoryAtRegister(DE), LdSrc::Register(A)),
        0x22 => LD(LdDst::MemoryAtHl(Some(IncDecOp::Inc)), LdSrc::Register(A)),
        0x32 => LD(LdDst::MemoryAtHl(Some(IncDecOp::Dec)), LdSrc::Register(A)),

        0x06 => LD(LdDst::Register(B), LdSrc::Immediate(mem.read(pc))),
        0x16 => LD(LdDst::Register(D), LdSrc::Immediate(mem.read(pc))),
        0x26 => LD(LdDst::Register(H), LdSrc::Immediate(mem.read(pc))),
        0x36 => LD(LdDst::MemoryAtRegister(HL), LdSrc::Immediate(mem.read(pc))),

        0x0A => LD(LdDst::Register(A), LdSrc::MemoryAtRegister(BC)),
        0x1A => LD(LdDst::Register(A), LdSrc::MemoryAtRegister(DE)),
        0x2A => LD(LdDst::Register(A), LdSrc::MemoryAtHl(Some(IncDecOp::Inc))),
        0x3A => LD(LdDst::Register(A), LdSrc::MemoryAtHl(Some(IncDecOp::Dec))),

        0x0E => LD(LdDst::Register(C), LdSrc::Immediate(mem.read(pc))),
        0x1E => LD(LdDst::Register(E), LdSrc::Immediate(mem.read(pc))),
        0x2E => LD(LdDst::Register(L), LdSrc::Immediate(mem.read(pc))),
        0x3E => LD(LdDst::Register(A), LdSrc::Immediate(mem.read(pc))),

        0x70..=0x75 | 0x77 => LD(LdDst::MemoryAtRegister(HL), (get_reg_param(opcode)).into()),

        0x40..=0x7F => {
            let (dest, src) = get_reg_params(opcode).unwrap_or_else(|| {
                panic!("This opcode doesn't take two registers as params: {opcode:X}")
            });

            LD(dest.into(), src.into())
        }

        0xE0 => LD(
            LdDst::IoMemory(IoMemoryOffset::Immediate(mem.read(pc))),
            LdSrc::Register(A),
        ),
        0xF0 => LD(
            LdDst::Register(A),
            LdSrc::IoMemory(IoMemoryOffset::Immediate(mem.read(pc))),
        ),
        0xE2 => LD(LdDst::IoMemory(IoMemoryOffset::C), LdSrc::Register(A)),
        0xF2 => LD(LdDst::Register(A), LdSrc::IoMemory(IoMemoryOffset::C)),

        0xEA => LD(LdDst::MemoryAt(mem.read_u16(pc)), LdSrc::Register(A)),
        0xFA => LD(LdDst::Register(A), LdSrc::MemoryAt(mem.read_u16(pc))),

        // 16bit loads
        0x01 => LD_long(
            LdLongDest::Register(BC),
            LdLongSrc::Immediate(mem.read_u16(pc)),
        ),
        0x11 => LD_long(
            LdLongDest::Register(DE),
            LdLongSrc::Immediate(mem.read_u16(pc)),
        ),
        0x21 => LD_long(
            LdLongDest::Register(HL),
            LdLongSrc::Immediate(mem.read_u16(pc)),
        ),
        0x31 => LD_long(LdLongDest::SP, LdLongSrc::Immediate(mem.read_u16(pc))),
        0x08 => LD_long(LdLongDest::MemoryAt(mem.read_u16(pc)), LdLongSrc::SP),
        0xF8 => LD_long(
            LdLongDest::Register(HL),
            LdLongSrc::SP_Offset(mem.read(pc) as i8),
        ),
        0xF9 => LD_long(LdLongDest::SP, LdLongSrc::HL),
        0xE8 => LD_long(LdLongDest::SP, LdLongSrc::Immediate(mem.read(pc) as u16)),

        // arithmetic operations
        0x80..=0x87 => ADD(get_reg_param(opcode).into()),
        0x88..=0x8F => ADC(get_reg_param(opcode).into()),
        0x90..=0x97 => SUB(get_reg_param(opcode).into()),
        0x98..=0x9F => SBC(get_reg_param(opcode).into()),
        0xA0..=0xA7 => AND(get_reg_param(opcode).into()),
        0xA8..=0xAF => XOR(get_reg_param(opcode).into()),
        0xB0..=0xB7 => OR(get_reg_param(opcode).into()),
        0xB8..=0xBF => CP(get_reg_param(opcode).into()),
        0xC6 => ADD(ArithmeticSource::Immediate(mem.read(pc))),
        0xD6 => SUB(ArithmeticSource::Immediate(mem.read(pc))),
        0xE6 => AND(ArithmeticSource::Immediate(mem.read(pc))),
        0xF6 => OR(ArithmeticSource::Immediate(mem.read(pc))),
        0xCE => ADC(ArithmeticSource::Immediate(mem.read(pc))),
        0xDE => SBC(ArithmeticSource::Immediate(mem.read(pc))),
        0xEE => XOR(ArithmeticSource::Immediate(mem.read(pc))),
        0xFE => CP(ArithmeticSource::Immediate(mem.read(pc))),

        // ADD 16 bit
        0x09 => ADD_hl(BC),
        0x19 => ADD_hl(DE),
        0x29 => ADD_hl(HL),
        0x39 => ADD_hl_sp,

        // INC/DEC: 8 bit
        0x04 => INCDEC(IncdecDestination::Register(B), IncDecOp::Inc),
        0x14 => INCDEC(IncdecDestination::Register(D), IncDecOp::Inc),
        0x24 => INCDEC(IncdecDestination::Register(H), IncDecOp::Inc),
        0x34 => INCDEC(IncdecDestination::MemoryAtHL, IncDecOp::Inc),
        0x0C => INCDEC(IncdecDestination::Register(C), IncDecOp::Inc),
        0x1C => INCDEC(IncdecDestination::Register(E), IncDecOp::Inc),
        0x2C => INCDEC(IncdecDestination::Register(L), IncDecOp::Inc),
        0x3C => INCDEC(IncdecDestination::Register(A), IncDecOp::Inc),

        0x05 => INCDEC(IncdecDestination::Register(B), IncDecOp::Dec),
        0x15 => INCDEC(IncdecDestination::Register(D), IncDecOp::Dec),
        0x25 => INCDEC(IncdecDestination::Register(H), IncDecOp::Dec),
        0x35 => INCDEC(IncdecDestination::MemoryAtHL, IncDecOp::Dec),
        0x0D => INCDEC(IncdecDestination::Register(C), IncDecOp::Dec),
        0x1D => INCDEC(IncdecDestination::Register(E), IncDecOp::Dec),
        0x2D => INCDEC(IncdecDestination::Register(L), IncDecOp::Dec),
        0x3D => INCDEC(IncdecDestination::Register(A), IncDecOp::Dec),

        // INC/DEC: 16 bit
        0x03 => INCDEC_long(IncdecLongDestination::Register(BC), IncDecOp::Inc),
        0x13 => INCDEC_long(IncdecLongDestination::Register(DE), IncDecOp::Inc),
        0x23 => INCDEC_long(IncdecLongDestination::Register(HL), IncDecOp::Inc),
        0x33 => INCDEC_long(IncdecLongDestination::SP, IncDecOp::Inc),

        0x0B => INCDEC_long(IncdecLongDestination::Register(BC), IncDecOp::Dec),
        0x1B => INCDEC_long(IncdecLongDestination::Register(DE), IncDecOp::Dec),
        0x2B => INCDEC_long(IncdecLongDestination::Register(HL), IncDecOp::Dec),
        0x3B => INCDEC_long(IncdecLongDestination::SP, IncDecOp::Dec),

        // bit operations
        0x07 => RLCA,
        0x17 => RLA,
        0x0F => RRCA,
        0x1F => RRA,
        0xCB => parse_bit_instr(mem.read(pc)),

        // Jumps
        0xC2 => JP(Some(Condition::NotZero), mem.read_u16(pc)),
        0xD2 => JP(Some(Condition::NotCarry), mem.read_u16(pc)),
        0xCA => JP(Some(Condition::Zero), mem.read_u16(pc)),
        0xDA => JP(Some(Condition::Carry), mem.read_u16(pc)),
        0xC3 => JP(None, mem.read_u16(pc)),
        0xE9 => JP_hl,

        0x18 => JR(None, mem.read(pc) as i8),
        0x28 => JR(Some(Condition::Zero), mem.read(pc) as i8),
        0x38 => JR(Some(Condition::Carry), mem.read(pc) as i8),
        0x20 => JR(Some(Condition::NotZero), mem.read(pc) as i8),
        0x30 => JR(Some(Condition::NotCarry), mem.read(pc) as i8),

        0xC7 => RST(0),
        0xD7 => RST(0x10),
        0xE7 => RST(0x20),
        0xF7 => RST(0x30),
        0xCF => RST(0x8),
        0xDF => RST(0x18),
        0xEF => RST(0x28),
        0xFF => RST(0x38),

        0xCD => CALL(None, mem.read_u16(pc)),
        0xC4 => CALL(Some(Condition::NotZero), mem.read_u16(pc)),
        0xD4 => CALL(Some(Condition::NotCarry), mem.read_u16(pc)),
        0xCC => CALL(Some(Condition::Zero), mem.read_u16(pc)),
        0xDC => CALL(Some(Condition::Carry), mem.read_u16(pc)),

        0xC9 => RET(None),
        0xC0 => RET(Some(Condition::NotZero)),
        0xD0 => RET(Some(Condition::NotCarry)),
        0xC8 => RET(Some(Condition::Zero)),
        0xD8 => RET(Some(Condition::Carry)),

        // interrupts
        0xF3 => DI,
        0xFB => EI,

        // misc
        0xC5 => PUSH(BC),
        0xD5 => PUSH(DE),
        0xE5 => PUSH(HL),
        0xF5 => PUSH(AF),

        0xC1 => POP(BC),
        0xD1 => POP(DE),
        0xE1 => POP(HL),
        0xF1 => POP(AF),

        0x2F => CPL,
        0x37 => SCF,
        0x3F => CCF,

        0xD3 | 0xE3 | 0xE4 | 0xF4 | 0xDB | 0xEB | 0xEC | 0xFC | 0xDD | 0xED | 0xFD => {
            panic!("Unused opcode, what do?");
        }
    }
}
fn parse_bit_instr(opcode: u8) -> Instruction {
    use Instruction::*;

    let param = get_reg_param(opcode).into();

    match opcode {
        0x00..=0x07 => RLC(param),
        0x08..=0x0F => RRC(param),
        0x10..=0x17 => RL(param),
        0x18..=0x1F => RR(param),
        0x20..=0x27 => SLA(param),
        0x28..=0x2F => SRA(param),
        0x30..=0x37 => SWAP(param),
        0x38..=0x3F => SRL(param),

        0x40..=0x47 => BIT(0, param),
        0x48..=0x4F => BIT(1, param),
        0x50..=0x57 => BIT(2, param),
        0x58..=0x5F => BIT(3, param),
        0x60..=0x67 => BIT(4, param),
        0x68..=0x6F => BIT(5, param),
        0x70..=0x77 => BIT(6, param),
        0x78..=0x7F => BIT(7, param),

        0x80..=0x87 => RES(0, param),
        0x88..=0x8F => RES(1, param),
        0x90..=0x97 => RES(2, param),
        0x98..=0x9F => RES(3, param),
        0xA0..=0xA7 => RES(4, param),
        0xA8..=0xAF => RES(5, param),
        0xB0..=0xB7 => RES(6, param),
        0xB8..=0xBF => RES(7, param),

        0xC0..=0xC7 => SET(0, param),
        0xC8..=0xCF => SET(1, param),
        0xD0..=0xD7 => SET(2, param),
        0xD8..=0xDF => SET(3, param),
        0xE0..=0xE7 => SET(4, param),
        0xE8..=0xEF => SET(5, param),
        0xF0..=0xF7 => SET(6, param),
        0xF8..=0xFF => SET(7, param),
    }
}

fn get_reg_param(opcode: u8) -> RegOrMemHl {
    use Register::*;

    match (opcode & 0x0F) % 8 {
        0 => RegOrMemHl::Register(B),
        1 => RegOrMemHl::Register(C),
        2 => RegOrMemHl::Register(D),
        3 => RegOrMemHl::Register(E),
        4 => RegOrMemHl::Register(H),
        5 => RegOrMemHl::Register(L),
        6 => RegOrMemHl::MemoryAtHl,
        7 => RegOrMemHl::Register(A),
        _ => unreachable!(),
    }
}

fn get_reg_params(opcode: u8) -> Option<(RegOrMemHl, RegOrMemHl)> {
    use Register::*;

    let high = opcode & 0xF0;
    let low = opcode & 0x0F;

    Some(match (high, low) {
        (40, 0..=7) => (RegOrMemHl::Register(B), get_reg_param(low)),
        (40, 8..) => (RegOrMemHl::Register(C), get_reg_param(low)),

        (50, 0..=7) => (RegOrMemHl::Register(D), get_reg_param(low)),
        (50, 8..) => (RegOrMemHl::Register(E), get_reg_param(low)),

        (60, 0..=7) => (RegOrMemHl::Register(H), get_reg_param(low)),
        (60, 8..) => (RegOrMemHl::Register(L), get_reg_param(low)),

        (70, 0..=7) => (RegOrMemHl::MemoryAtHl, get_reg_param(low)),
        (70, 8..) => (RegOrMemHl::Register(A), get_reg_param(low)),

        _ => return None,
    })
}
