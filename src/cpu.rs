use crate::{
    instruction::{
        ArithmeticSource, BitOperand, Condition, IncDecOp, IncdecDestination,
        IncdecLongDestination, Instruction, IoMemoryOffset, LoadDestination, LoadLongDestination,
        LoadLongSource, LoadSource,
    },
    mmu::Mmu,
    opcode_parsing_helper::RegOrMemHl,
    registers::{Flag, LongRegister, Register, Registers},
};

struct Cpu {
    reg: Registers,
    pc: u16,
    sp: u16,
    mem: Box<Mmu>,
    interrupts_enabled: bool,
}

impl Cpu {
    pub fn step(&mut self) {
        let instr = self.parse_next_instr();
        dbg!(instr.clone());
        self.execute(instr);
    }

    fn execute(&mut self, instr: Instruction) {
        use Instruction::*;

        match instr {
            NOP => todo!(),
            STOP => todo!(),
            HALT => todo!(),

            LD(destination, source) => self.load(destination, source),
            LD_long(destination, source) => self.load_long(destination, source),

            ADD(src) => self.add(src, false),
            ADC(src) => self.add(src, true),

            SUB(src) => self.sub(src, false),
            SBC(src) => self.sub(src, true),

            AND(src) => self.and(src),
            XOR(src) => self.xor(src),
            OR(src) => self.or(src),
            CP(src) => self.cp(src),

            ADD_hl(src) => self.add_hl(src),
            ADD_hl_sp => self.add_hl_sp(),

            INCDEC(destination, op) => self.incdec(destination, op),
            INCDEC_long(destination, op) => self.incdec_long(destination, op),

            RLCA => self.rlca(),
            RLA => self.rla(),
            RRCA => self.rrca(),
            RRA => self.rra(),

            RLC(operand) => self.rotate_left(operand, false),
            RRC(operand) => self.rotate_right(operand, false),
            RL(operand) => self.rotate_left(operand, true),
            RR(operand) => self.rotate_right(operand, true),
            SLA(operand) => self.shift_left(operand, false),
            SRA(operand) => self.shift_right_arithmetic(operand),
            SWAP(operand) => self.swap(operand),
            SRL(operand) => self.shift_right(operand, false),

            BIT(bit, operand) => self.bit(bit, operand),
            RES(bit, operand) => self.res(bit, operand),
            SET(bit, operand) => self.set(bit, operand),

            JP(cond, addr) => self.jump(cond, addr),
            JP_hl => {
                self.pc = self.reg.read_long(LongRegister::HL);
            }
            JR(cond, offset) => self.relative_jump(cond, offset),

            RST(offset) => self.call(None, offset as u16),
            CALL(cond, addr) => self.call(cond, addr),
            RET(cond) => self.ret(cond),

            EI => self.interrupts_enabled = true,
            DI => self.interrupts_enabled = false,

            PUSH(reg) => self.push(reg),
            POP(reg) => self.pop(reg),

            CPL => self.cpl(),
            SCF => self.scf(),
            CCF => self.ccf(),
        }
    }

    fn bit(&mut self, bit: u8, operand: BitOperand) {
        let val = self.read_bit_operand(operand);

        let masked = val & (1 << bit);

        self.reg.set_flag(Flag::HalfCarry, true);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::Zero, masked > 0);
    }

    fn res(&mut self, bit: u8, operand: BitOperand) {
        let val = self.read_bit_operand(operand);

        let bitmask = !(1 << bit);
        let result = val & bitmask;

        self.write_bit_operand(operand, result);
    }

    fn set(&mut self, bit: u8, operand: BitOperand) {
        let val = self.read_bit_operand(operand);

        let result = val | (1 << bit);

        self.write_bit_operand(operand, result);
    }

    fn swap(&mut self, operand: BitOperand) {
        let val = self.read_bit_operand(operand);

        let low_nibble = (val & 0x0F) << 4;
        let high_nibble = (val & 0xF0) >> 4;
        let result = high_nibble | low_nibble;

        self.write_bit_operand(operand, result);

        self.reg
            .set_flags(&[Flag::HalfCarry, Flag::Negative, Flag::Carry], false);
        self.reg.set_flag(Flag::Zero, result == 0);
    }

    fn read_bit_operand(&self, op: BitOperand) -> u8 {
        match op {
            BitOperand::MemoryAtHl => self.mem.read(self.reg.read_long(LongRegister::HL)),
            BitOperand::Register(reg) => self.reg.read(reg),
        }
    }

    fn write_bit_operand(&mut self, op: BitOperand, val: u8) {
        match op {
            BitOperand::MemoryAtHl => self.mem.write(self.reg.read_long(LongRegister::HL), val),
            BitOperand::Register(reg) => self.reg.write(reg, val),
        };
    }

    fn shift_right_arithmetic(&mut self, operand: BitOperand) {
        let val = self.read_bit_operand(operand);

        let carry_bit = val & 1;
        let high_bit = val & 0b1000_0000;
        let result = (val >> 1) | high_bit;

        self.write_bit_operand(operand, result);

        self.reg
            .set_flags(&[Flag::HalfCarry, Flag::Negative], false);
        self.reg.set_flag(Flag::Carry, carry_bit > 0);
        self.reg.set_flag(Flag::Zero, result == 0);
    }

    fn shift_right(&mut self, operand: BitOperand, through_carry: bool) {
        let val = self.read_bit_operand(operand);

        let mut carry_bit = val & 1;
        if through_carry && self.reg.get_flag(Flag::Carry) {
            carry_bit = 1;
        }

        let result = val >> 1;

        self.write_bit_operand(operand, result);

        self.reg
            .set_flags(&[Flag::HalfCarry, Flag::Negative], false);
        self.reg.set_flag(Flag::Carry, carry_bit > 0);
        self.reg.set_flag(Flag::Zero, result == 0);
    }

    fn shift_left(&mut self, operand: BitOperand, through_carry: bool) {
        let val = self.read_bit_operand(operand);

        let mut carry_bit = (val & 0b1000_0000) >> 7;
        if through_carry && self.reg.get_flag(Flag::Carry) {
            carry_bit = 1;
        }

        let result = val << 1;

        self.write_bit_operand(operand, result);

        self.reg
            .set_flags(&[Flag::HalfCarry, Flag::Negative], false);
        self.reg.set_flag(Flag::Carry, carry_bit > 0);
        self.reg.set_flag(Flag::Zero, result == 0);
    }

    fn rotate_right(&mut self, operand: BitOperand, include_carry: bool) {
        let val = self.read_bit_operand(operand);

        let mut carry_bit = val & 1;
        if include_carry && self.reg.get_flag(Flag::Carry) {
            carry_bit = 1;
        }

        let result = (val >> 1) | (carry_bit << 7);

        self.write_bit_operand(operand, result);

        self.reg
            .set_flags(&[Flag::HalfCarry, Flag::Negative], false);
        self.reg.set_flag(Flag::Carry, carry_bit > 0);
        self.reg.set_flag(Flag::Zero, result == 0);
    }

    fn rotate_left(&mut self, operand: BitOperand, include_carry: bool) {
        let val = self.read_bit_operand(operand);

        let mut carry_bit = (val & 0b1000_0000) >> 7;
        if include_carry && self.reg.get_flag(Flag::Carry) {
            carry_bit = 1;
        }

        let result = (val << 1) | carry_bit;

        self.write_bit_operand(operand, result);

        self.reg
            .set_flags(&[Flag::HalfCarry, Flag::Negative], false);
        self.reg.set_flag(Flag::Carry, carry_bit > 0);
        self.reg.set_flag(Flag::Zero, result == 0);
    }

    fn load(&mut self, dest: LoadDestination, source: LoadSource) {
        fn incdec_hl(cpu: &mut Cpu, op: IncDecOp) {
            let hl_val = cpu.reg.read_long(LongRegister::HL);

            let new_hl = match op {
                IncDecOp::Inc => hl_val.wrapping_add(1),
                IncDecOp::Dec => hl_val.wrapping_sub(1),
            };

            cpu.reg.write_long(LongRegister::HL, new_hl);
        }

        let val = match source {
            LoadSource::Immediate(x) => x,
            LoadSource::MemoryAtRegister(reg) => self.mem.read(self.reg.read_long(reg)),
            LoadSource::MemoryAtHl(_) => self.mem.read(self.reg.read_long(LongRegister::HL)),
            LoadSource::Register(reg) => self.reg.read(reg),
            LoadSource::IoMemory(IoMemoryOffset::Immediate(offset)) => {
                self.mem.read(0xFF00 + offset as u16)
            }

            LoadSource::IoMemory(IoMemoryOffset::C) => {
                self.mem.read(0xFF00 + self.reg.read(Register::C) as u16)
            }
            LoadSource::MemoryAt(addr) => self.mem.read(addr),
        };

        if let LoadSource::MemoryAtHl(Some(op)) = source {
            incdec_hl(self, op);
        }

        match dest {
            LoadDestination::Register(reg) => self.reg.write(reg, val),
            LoadDestination::MemoryAtHl(_) => {
                self.mem.write(self.reg.read_long(LongRegister::HL), val);

                if let LoadDestination::MemoryAtHl(Some(op)) = dest {
                    incdec_hl(self, op);
                }
            }
            LoadDestination::MemoryAt(addr) => self.mem.write(addr, val),
            LoadDestination::MemoryAtRegister(reg) => {
                self.mem.write(self.reg.read_long(reg), val);
            }
            LoadDestination::IoMemory(IoMemoryOffset::C) => {
                self.mem
                    .write(0xFF00 + self.reg.read(Register::C) as u16, val);
            }
            LoadDestination::IoMemory(IoMemoryOffset::Immediate(offset)) => {
                self.mem.write(0xFF00 + offset as u16, val);
            }
        }
    }

    fn load_long(&mut self, dest: LoadLongDestination, source: LoadLongSource) {
        let val = match source {
            LoadLongSource::Immediate(x) => x,
            LoadLongSource::SP => self.sp,
            LoadLongSource::SP_Offset(offset) => self.sp.wrapping_add_signed(offset as i16),
            LoadLongSource::HL => self.reg.read_long(LongRegister::HL),
        };

        match dest {
            LoadLongDestination::Register(reg) => self.reg.write_long(reg, val),
            LoadLongDestination::SP => self.sp = val,
            LoadLongDestination::MemoryAt(addr) => self.mem.write_u16(addr, val),
        };
    }

    fn incdec(&mut self, dest: IncdecDestination, op: IncDecOp) {
        let val = match dest {
            IncdecDestination::Register(reg) => self.reg.read(reg),
            IncdecDestination::MemoryAtHL => self.mem.read(self.reg.read_long(LongRegister::HL)),
        };

        let result = if op == IncDecOp::Inc {
            val.wrapping_add(1)
        } else {
            val.wrapping_sub(1)
        };

        self.reg.set_flag(Flag::Zero, result == 0);
        self.reg.set_flag(Flag::Zero, result == 0);
        self.reg.set_flag(Flag::Negative, op == IncDecOp::Dec);

        let halfcarry = if op == IncDecOp::Inc {
            val & 0x0F == 0x0F
        } else {
            val == 0
        };
        self.reg.set_flag(Flag::HalfCarry, halfcarry);

        match dest {
            IncdecDestination::Register(reg) => self.reg.write(reg, result),
            IncdecDestination::MemoryAtHL => {
                self.mem.write(self.reg.read_long(LongRegister::HL), result)
            }
        }
    }

    fn incdec_long(&mut self, dest: IncdecLongDestination, op: IncDecOp) {
        let val = match dest {
            IncdecLongDestination::Register(reg) => self.reg.read_long(reg),
            IncdecLongDestination::SP => self.sp,
        };

        let result = if op == IncDecOp::Inc {
            val.wrapping_add(1)
        } else {
            val.wrapping_sub(1)
        };

        self.reg.set_flag(Flag::Zero, result == 0);
        self.reg.set_flag(Flag::Negative, op == IncDecOp::Dec);

        let halfcarry = if op == IncDecOp::Inc {
            val & 0x0F == 0x0F
        } else {
            val == 0
        };
        self.reg.set_flag(Flag::HalfCarry, halfcarry);

        match dest {
            IncdecLongDestination::Register(reg) => self.reg.write_long(reg, result),
            IncdecLongDestination::SP => self.sp = result,
        }
    }

    fn add(&mut self, source: ArithmeticSource, add_with_carry: bool) {
        let carry = if add_with_carry && self.reg.get_flag(Flag::Carry) {
            1
        } else {
            0
        };
        let val = self.get_source_value(source);

        let (result, overflow) = {
            let (result, overflow1) = self.reg.read(Register::A).overflowing_add(val);
            let (result, overflow2) = result.overflowing_add(carry);
            (result, overflow1 || overflow2)
        };

        self.reg.set_flag(Flag::Carry, overflow);
        self.reg.set_flag(Flag::Zero, result == 0);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, {
            let (reg_nibble, src_nibble) = (self.reg.read(Register::A) & 0x0F, val & 0x0F);

            let nibble_result = reg_nibble + src_nibble;
            nibble_result & 0x10 == 0x10
        });

        self.reg.write(Register::A, result);
    }

    fn sub(&mut self, source: ArithmeticSource, sub_with_carry: bool) {
        let carry = if sub_with_carry && self.reg.get_flag(Flag::Carry) {
            1
        } else {
            0
        };
        let val = self.get_source_value(source);

        let (result, overflow) = {
            let (result, overflow1) = self.reg.read(Register::A).overflowing_sub(val);
            let (result, overflow2) = result.overflowing_sub(carry);
            (result, overflow1 || overflow2)
        };

        self.reg.set_flag(Flag::Carry, overflow);
        self.reg.set_flag(Flag::Zero, result == 0);
        self.reg.set_flag(Flag::Negative, true);
        self.reg.set_flag(Flag::HalfCarry, {
            let (reg_nibble, src_nibble) = (self.reg.read(Register::A) & 0x0F, val & 0x0F);

            // TODO: Are these semantics really correct?
            let (result, overflow1) = reg_nibble.overflowing_sub(src_nibble);
            let (_, overflow2) = result.overflowing_sub(carry);

            overflow1 || overflow2
        });

        self.reg.write(Register::A, result);
    }

    fn and(&mut self, source: ArithmeticSource) {
        let val = self.get_source_value(source);

        let result = self.reg.read(Register::A) & val;
        self.reg.set_flag(Flag::Carry, false);
        self.reg.set_flag(Flag::Zero, result == 0);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, true);

        self.reg.write(Register::A, result);
    }

    fn xor(&mut self, source: ArithmeticSource) {
        let val = self.get_source_value(source);

        let result = self.reg.read(Register::A) ^ val;
        self.reg.set_flag(Flag::Carry, false);
        self.reg.set_flag(Flag::Zero, result == 0);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, false);

        self.reg.write(Register::A, result);
    }

    fn or(&mut self, source: ArithmeticSource) {
        let val = self.get_source_value(source);

        self.reg
            .write(Register::A, self.reg.read(Register::A) | val);
        self.reg.set_flag(Flag::Carry, false);
        self.reg
            .set_flag(Flag::Zero, self.reg.read(Register::A) == 0);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, false);
    }

    fn cp(&mut self, source: ArithmeticSource) {
        let val = self.get_source_value(source);

        let (new_val, overflow) = self.reg.read(Register::A).overflowing_sub(val);
        self.reg.set_flag(Flag::Carry, overflow);
        self.reg.set_flag(Flag::Zero, new_val == 0);
        self.reg.set_flag(Flag::Negative, true);
        self.reg.set_flag(Flag::HalfCarry, {
            let (reg_nibble, src_nibble) = (self.reg.read(Register::A) & 0x0F, val & 0x0F);
            let (_, half_overflow) = reg_nibble.overflowing_sub(src_nibble);
            half_overflow
        });
    }

    fn add_hl(&mut self, source: LongRegister) {
        let val = self.reg.read_long(source);

        let (result, overflow) = self.reg.read_long(LongRegister::HL).overflowing_add(val);

        self.reg.set_flag(Flag::Carry, overflow);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, {
            let (reg_nibble, src_nibble) = (
                (self.reg.read_long(LongRegister::HL) >> 8) & 0x0F,
                (val >> 8) & 0x0F,
            );

            let nibble_result = reg_nibble + src_nibble;
            nibble_result & 0x10 == 0x10
        });

        self.reg.write_long(LongRegister::HL, result);
    }

    // TODO: Recombine this with add_hl, add SP to LongRegister::
    fn add_hl_sp(&mut self) {
        let val = self.sp;

        let (result, overflow) = self.reg.read_long(LongRegister::HL).overflowing_add(val);

        self.reg.set_flag(Flag::Carry, overflow);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, {
            let (reg_nibble, src_nibble) = (
                (self.reg.read_long(LongRegister::HL) >> 8) & 0x0F,
                (val >> 8) & 0x0F,
            );

            let nibble_result = reg_nibble + src_nibble;
            nibble_result & 0x10 == 0x10
        });

        self.reg.write_long(LongRegister::HL, result);
    }

    fn rla(&mut self) {
        let carry = self.reg.get_flag_bit(Flag::Carry);
        let val = self.reg.read(Register::A);
        let newcarry = val >> 7;
        self.reg.write(Register::A, (val << 1) | carry);

        self.reg.set_flag(Flag::Carry, newcarry == 1);
        self.reg
            .set_flags(&[Flag::HalfCarry, Flag::Zero, Flag::Negative], false);
    }

    fn rlca(&mut self) {
        let val = self.reg.read(Register::A);
        let carry = val >> 7;
        self.reg.write(Register::A, (val << 1) | carry);

        self.reg.set_flag(Flag::Carry, carry == 1);
        self.reg
            .set_flags(&[Flag::HalfCarry, Flag::Zero, Flag::Negative], false);
    }

    fn rra(&mut self) {
        let carry = self.reg.get_flag_bit(Flag::Carry);

        let val = self.reg.read(Register::A);
        let newcarry = val & 1;
        self.reg.write(Register::A, (val >> 1) | (carry << 7));

        self.reg.set_flag(Flag::Carry, newcarry == 1);
        self.reg
            .set_flags(&[Flag::HalfCarry, Flag::Zero, Flag::Negative], false);
    }

    fn rrca(&mut self) {
        let val = self.reg.read(Register::A);
        let carry = val & 1;
        self.reg.write(Register::A, (val >> 1) | (carry << 7));

        self.reg.set_flag(Flag::Carry, carry == 1);
        self.reg
            .set_flags(&[Flag::HalfCarry, Flag::Zero, Flag::Negative], false);
    }

    fn push(&mut self, reg: LongRegister) {
        let val = self.reg.read_long(reg);
        self.mem.write(self.sp - 1, (val >> 8) as u8);
        self.mem.write(self.sp - 2, val as u8);
        self.sp -= 2;
    }

    fn pop(&mut self, reg: LongRegister) {
        let lo = self.mem.read(self.sp) as u16;
        let hi = self.mem.read(self.sp + 1) as u16;
        self.reg.write_long(reg, (hi << 8) | lo);
        self.sp += 2;
    }

    fn get_source_value(&self, source: ArithmeticSource) -> u8 {
        match source {
            ArithmeticSource::Register(reg) => self.reg.read(reg),
            ArithmeticSource::Immediate(x) => x,
            ArithmeticSource::MemoryAtHl => self.mem.read(self.reg.read_long(LongRegister::HL)),
        }
    }

    // TODO: Move this out of the cpu
    fn parse_next_instr(&mut self) -> Instruction {
        use Instruction::*;
        use LoadDestination as LdDst;
        use LoadLongDestination as LdLongDest;
        use LoadLongSource as LdLongSrc;
        use LoadSource as LdSrc;
        use LongRegister::*;
        use Register::*;

        let opcode = self.mem.read(self.pc);
        self.pc += 1;

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

            0x06 => LD(LdDst::Register(B), LdSrc::Immediate(self.mem.read(self.pc))),
            0x16 => LD(LdDst::Register(D), LdSrc::Immediate(self.mem.read(self.pc))),
            0x26 => LD(LdDst::Register(H), LdSrc::Immediate(self.mem.read(self.pc))),
            0x36 => LD(
                LdDst::MemoryAtRegister(HL),
                LdSrc::Immediate(self.mem.read(self.pc)),
            ),

            0x0A => LD(LdDst::Register(A), LdSrc::MemoryAtRegister(BC)),
            0x1A => LD(LdDst::Register(A), LdSrc::MemoryAtRegister(DE)),
            0x2A => LD(LdDst::Register(A), LdSrc::MemoryAtHl(Some(IncDecOp::Inc))),
            0x3A => LD(LdDst::Register(A), LdSrc::MemoryAtHl(Some(IncDecOp::Dec))),

            0x0E => LD(LdDst::Register(C), LdSrc::Immediate(self.mem.read(self.pc))),
            0x1E => LD(LdDst::Register(E), LdSrc::Immediate(self.mem.read(self.pc))),
            0x2E => LD(LdDst::Register(L), LdSrc::Immediate(self.mem.read(self.pc))),
            0x3E => LD(LdDst::Register(A), LdSrc::Immediate(self.mem.read(self.pc))),

            // TODO: Combine these with the ones below;
            // get_reg_param would have to return a source/dest instead of Register
            // maybe implement from/into for specific param types
            0x70..=0x75 | 0x77 => LD(LdDst::MemoryAtRegister(HL), (get_reg_param(opcode)).into()),

            0x40..=0x7F => {
                let (dest, src) = get_reg_params(opcode).unwrap_or_else(|| {
                    panic!("This opcode doesn't take two registers as params: {opcode:X}")
                });

                LD(dest.into(), src.into())
            }

            0xE0 => LD(
                LdDst::IoMemory(IoMemoryOffset::Immediate(self.mem.read(self.pc))),
                LdSrc::Register(A),
            ),
            0xF0 => LD(
                LdDst::Register(A),
                LdSrc::IoMemory(IoMemoryOffset::Immediate(self.mem.read(self.pc))),
            ),
            0xE2 => LD(LdDst::IoMemory(IoMemoryOffset::C), LdSrc::Register(A)),
            0xF2 => LD(LdDst::Register(A), LdSrc::IoMemory(IoMemoryOffset::C)),

            0xEA => LD(
                LdDst::MemoryAt(self.mem.read_u16(self.pc)),
                LdSrc::Register(A),
            ),
            0xFA => LD(
                LdDst::Register(A),
                LdSrc::MemoryAt(self.mem.read_u16(self.pc)),
            ),

            // 16bit loads
            0x01 => LD_long(
                LdLongDest::Register(BC),
                LdLongSrc::Immediate(self.mem.read_u16(self.pc)),
            ),
            0x11 => LD_long(
                LdLongDest::Register(DE),
                LdLongSrc::Immediate(self.mem.read_u16(self.pc)),
            ),
            0x21 => LD_long(
                LdLongDest::Register(HL),
                LdLongSrc::Immediate(self.mem.read_u16(self.pc)),
            ),
            0x31 => LD_long(
                LdLongDest::SP,
                LdLongSrc::Immediate(self.mem.read_u16(self.pc)),
            ),
            0x08 => LD_long(
                LdLongDest::MemoryAt(self.mem.read_u16(self.pc)),
                LdLongSrc::SP,
            ),
            0xF8 => LD_long(
                LdLongDest::Register(HL),
                LdLongSrc::SP_Offset(self.mem.read(self.pc) as i8),
            ),
            0xF9 => LD_long(LdLongDest::SP, LdLongSrc::HL),
            0xE8 => LD_long(
                LdLongDest::SP,
                LdLongSrc::Immediate(self.mem.read(self.pc) as u16),
            ),

            // arithmetic operations
            0x80..=0x87 => ADD(get_reg_param(opcode).into()),
            0x88..=0x8F => ADC(get_reg_param(opcode).into()),
            0x90..=0x97 => SUB(get_reg_param(opcode).into()),
            0x98..=0x9F => SBC(get_reg_param(opcode).into()),
            0xA0..=0xA7 => AND(get_reg_param(opcode).into()),
            0xA8..=0xAF => XOR(get_reg_param(opcode).into()),
            0xB0..=0xB7 => OR(get_reg_param(opcode).into()),
            0xB8..=0xBF => CP(get_reg_param(opcode).into()),
            0xC6 => ADD(ArithmeticSource::Immediate(self.mem.read(self.pc))),
            0xD6 => SUB(ArithmeticSource::Immediate(self.mem.read(self.pc))),
            0xE6 => AND(ArithmeticSource::Immediate(self.mem.read(self.pc))),
            0xF6 => OR(ArithmeticSource::Immediate(self.mem.read(self.pc))),
            0xCE => ADC(ArithmeticSource::Immediate(self.mem.read(self.pc))),
            0xDE => SBC(ArithmeticSource::Immediate(self.mem.read(self.pc))),
            0xEE => XOR(ArithmeticSource::Immediate(self.mem.read(self.pc))),
            0xFE => CP(ArithmeticSource::Immediate(self.mem.read(self.pc))),

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
            0xCB => self.parse_bit_instr(),

            // Jumps
            0xC2 => JP(Some(Condition::NotZero), self.mem.read_u16(self.pc)),
            0xD2 => JP(Some(Condition::NotCarry), self.mem.read_u16(self.pc)),
            0xCA => JP(Some(Condition::Zero), self.mem.read_u16(self.pc)),
            0xDA => JP(Some(Condition::Carry), self.mem.read_u16(self.pc)),
            0xC3 => JP(None, self.mem.read_u16(self.pc)),
            0xE9 => JP_hl,

            0x18 => JR(None, self.mem.read(self.pc) as i8),
            0x28 => JR(Some(Condition::Zero), self.mem.read(self.pc) as i8),
            0x38 => JR(Some(Condition::Carry), self.mem.read(self.pc) as i8),
            0x20 => JR(Some(Condition::NotZero), self.mem.read(self.pc) as i8),
            0x30 => JR(Some(Condition::NotCarry), self.mem.read(self.pc) as i8),

            0xC7 => RST(0),
            0xD7 => RST(0x10),
            0xE7 => RST(0x20),
            0xF7 => RST(0x30),
            0xCF => RST(0x8),
            0xDF => RST(0x18),
            0xEF => RST(0x28),
            0xFF => RST(0x38),

            0xCD => CALL(None, self.mem.read_u16(self.pc)),
            0xC4 => CALL(Some(Condition::NotZero), self.mem.read_u16(self.pc)),
            0xD4 => CALL(Some(Condition::NotCarry), self.mem.read_u16(self.pc)),
            0xCC => CALL(Some(Condition::Zero), self.mem.read_u16(self.pc)),
            0xDC => CALL(Some(Condition::Carry), self.mem.read_u16(self.pc)),

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

    fn parse_bit_instr(&mut self) -> Instruction {
        use Instruction::*;

        let opcode = self.mem.read(self.pc + 1);
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

    fn cpl(&mut self) {
        self.reg.write(Register::A, !self.reg.read(Register::A));
        self.reg.set_flags(&[Flag::HalfCarry, Flag::Negative], true);
    }

    fn ccf(&mut self) {
        self.reg
            .set_flag(Flag::Carry, !self.reg.get_flag(Flag::Carry));
        self.reg.set_flags(&[Flag::HalfCarry, Flag::Carry], false);
    }

    fn scf(&mut self) {
        self.reg.set_flag(Flag::Carry, true);
        self.reg.set_flags(&[Flag::HalfCarry, Flag::Carry], false);
    }

    fn eval_condition(&self, cond: Condition) -> bool {
        match cond {
            Condition::Zero => self.reg.get_flag(Flag::Zero),
            Condition::Carry => self.reg.get_flag(Flag::Carry),
            Condition::NotZero => !self.reg.get_flag(Flag::Zero),
            Condition::NotCarry => !self.reg.get_flag(Flag::Carry),
        }
    }

    fn jump(&mut self, cond: Option<Condition>, addr: u16) {
        if cond.is_none() || self.eval_condition(cond.unwrap()) {
            self.pc = addr;
        }
    }

    fn relative_jump(&mut self, cond: Option<Condition>, offset: i8) {
        if cond.is_none() || self.eval_condition(cond.unwrap()) {
            self.pc = self.pc.checked_add_signed(offset as i16).unwrap();
        }
    }

    fn call(&mut self, cond: Option<Condition>, addr: u16) {
        if cond.is_none() || self.eval_condition(cond.unwrap()) {
            self.push_pc_stack();
            self.pc = addr;
        }
    }

    fn ret(&mut self, cond: Option<Condition>) {
        if cond.is_none() || self.eval_condition(cond.unwrap()) {
            self.pop_stack_pc();
        }
    }

    fn push_pc_stack(&mut self) {
        self.mem.write(self.sp - 1, (self.pc >> 8) as u8);
        self.mem.write(self.sp - 2, self.pc as u8);
        self.sp -= 2;
    }

    fn pop_stack_pc(&mut self) {
        let new_pc = (self.mem.read(self.sp + 1) as u16) << 8 | (self.mem.read(self.sp) as u16);
        self.sp += 2;
        self.pc = new_pc;
    }
}

// TODO: Naming, this is also used for bit operations where it's not the -second- param necessarily
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
