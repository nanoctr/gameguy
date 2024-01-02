use crate::{
    instruction::{
        BitOperand, Condition, Destination, IncdecDestination, IncdecLongDestination, IncrementOp,
        Instruction, LoadHighOperand, LongDestination, LongSource, Source,
    },
    mmu::Mmu,
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
    pub fn step(&mut self) -> Result<(), String> {
        let instr = self.parse_next_instr()?;
        self.execute(instr)?;

        Ok(())
    }

    fn execute(&mut self, instr: Instruction) -> Result<u16, String> {
        use Instruction::*;

        match instr {
            NOP => {}
            STOP => todo!(),

            LD(destination, source) => self.load(destination, source),
            LD_long(destination, source) => self.load_long(destination, source),
            LD_sp_hl => self.ld_sp_hl(),
            LD_hl_sp_n(offset) => self.ld_hl_sp_n(offset),
            LD_a_mem(addr) => self.ld_a_mem(addr),
            LD_high(dest, src) => self.load_high(dest, src),

            INCDEC(destination, op) => self.incdec(destination, op),
            INCDEC_long(destination, op) => self.incdec_long(destination, op),

            ADD(src) => self.add(src, false),
            ADC(src) => self.add(src, true),

            SUB(src) => self.sub(src, false),
            SBC(src) => self.sub(src, true),

            AND(src) => self.and(src),
            XOR(src) => self.xor(src),
            OR(src) => self.or(src),
            CP(src) => self.cp(src),

            ADD_hl(src) => self.add_hl(src),
            ADD_hl_hl => self.add_hl_hl(),

            JP(cond, addr) => self.jump(cond, addr),
            JP_hl => {
                self.pc = self.reg.read_long(LongRegister::HL);
            }
            JR(cond, offset) => self.relative_jump(cond, offset),

            RLA => self.rla(),
            RLCA => self.rlca(),
            RRA => self.rra(),
            RRCA => self.rrca(),

            CPL => self.cpl(),

            CCF => self.ccf(),
            SCF => self.scf(),

            PUSH(reg) => self.push(reg),
            POP(reg) => self.pop(reg),

            RST(offset) => self.call(None, offset as u16),
            CALL(cond, addr) => self.call(cond, addr),

            RET(cond) => self.ret(cond),

            // bit operations
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

            EI => self.interrupts_enabled = true,
            DI => self.interrupts_enabled = false,

            x => return Err(format!("Instruction not implemented: {x:?}")),
        }

        if !instr.is_jump() {
            self.pc += instr.length()?
        }

        instr.cycles()
    }

    fn bit(&mut self, bit: u8, operand: BitOperand) {
        let val = match operand {
            BitOperand::MemoryAtHL => self.mem.read(self.reg.read_long(LongRegister::HL)),
            BitOperand::Register(reg) => self.reg.read(reg),
        };

        let masked = val & (1 << bit);

        self.reg.set_flag(Flag::HalfCarry, true);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::Zero, masked > 0);
    }

    fn res(&mut self, bit: u8, operand: BitOperand) {
        let val = match operand {
            BitOperand::MemoryAtHL => self.mem.read(self.reg.read_long(LongRegister::HL)),
            BitOperand::Register(reg) => self.reg.read(reg),
        };

        let bitmask = !(1 << bit);
        let result = val & bitmask;

        match operand {
            BitOperand::MemoryAtHL => self.mem.write(self.reg.read_long(LongRegister::HL), result),
            BitOperand::Register(reg) => self.reg.write(reg, result),
        };
    }

    fn set(&mut self, bit: u8, operand: BitOperand) {
        let val = match operand {
            BitOperand::MemoryAtHL => self.mem.read(self.reg.read_long(LongRegister::HL)),
            BitOperand::Register(reg) => self.reg.read(reg),
        };

        let result = val | (1 << bit);

        match operand {
            BitOperand::MemoryAtHL => self.mem.write(self.reg.read_long(LongRegister::HL), result),
            BitOperand::Register(reg) => self.reg.write(reg, result),
        };
    }

    fn swap(&mut self, operand: BitOperand) {
        let val = match operand {
            BitOperand::MemoryAtHL => self.mem.read(self.reg.read_long(LongRegister::HL)),
            BitOperand::Register(reg) => self.reg.read(reg),
        };

        let low_nibble = (val & 0x0F) << 4;
        let high_nibble = (val & 0xF0) >> 4;
        let result = high_nibble | low_nibble;

        match operand {
            BitOperand::MemoryAtHL => self.mem.write(self.reg.read_long(LongRegister::HL), result),
            BitOperand::Register(reg) => self.reg.write(reg, result),
        };

        self.reg
            .set_flags(&[Flag::HalfCarry, Flag::Negative, Flag::Carry], false);
        self.reg.set_flag(Flag::Zero, result == 0);
    }

    fn shift_right_arithmetic(&mut self, operand: BitOperand) {
        let val = match operand {
            BitOperand::MemoryAtHL => self.mem.read(self.reg.read_long(LongRegister::HL)),
            BitOperand::Register(reg) => self.reg.read(reg),
        };

        let carry_bit = val & 1;
        let high_bit = val & 0b1000_0000;
        let result = (val >> 1) | high_bit;

        match operand {
            BitOperand::MemoryAtHL => self.mem.write(self.reg.read_long(LongRegister::HL), result),
            BitOperand::Register(reg) => self.reg.write(reg, result),
        };

        self.reg
            .set_flags(&[Flag::HalfCarry, Flag::Negative], false);
        self.reg.set_flag(Flag::Carry, carry_bit > 0);
        self.reg.set_flag(Flag::Zero, result == 0);
    }

    fn shift_right(&mut self, operand: BitOperand, through_carry: bool) {
        let val = match operand {
            BitOperand::MemoryAtHL => self.mem.read(self.reg.read_long(LongRegister::HL)),
            BitOperand::Register(reg) => self.reg.read(reg),
        };

        let mut carry_bit = val & 1;
        if through_carry && self.reg.get_flag(Flag::Carry) {
            carry_bit = 1;
        }

        let result = val >> 1;

        match operand {
            BitOperand::MemoryAtHL => self.mem.write(self.reg.read_long(LongRegister::HL), result),
            BitOperand::Register(reg) => self.reg.write(reg, result),
        };

        self.reg
            .set_flags(&[Flag::HalfCarry, Flag::Negative], false);
        self.reg.set_flag(Flag::Carry, carry_bit > 0);
        self.reg.set_flag(Flag::Zero, result == 0);
    }

    fn shift_left(&mut self, operand: BitOperand, through_carry: bool) {
        let val = match operand {
            BitOperand::MemoryAtHL => self.mem.read(self.reg.read_long(LongRegister::HL)),
            BitOperand::Register(reg) => self.reg.read(reg),
        };

        let mut carry_bit = (val & 0b1000_0000) >> 7;
        if through_carry && self.reg.get_flag(Flag::Carry) {
            carry_bit = 1;
        }

        let result = val << 1;

        match operand {
            BitOperand::MemoryAtHL => self.mem.write(self.reg.read_long(LongRegister::HL), result),
            BitOperand::Register(reg) => self.reg.write(reg, result),
        };

        self.reg
            .set_flags(&[Flag::HalfCarry, Flag::Negative], false);
        self.reg.set_flag(Flag::Carry, carry_bit > 0);
        self.reg.set_flag(Flag::Zero, result == 0);
    }

    fn rotate_right(&mut self, operand: BitOperand, include_carry: bool) {
        let val = match operand {
            BitOperand::MemoryAtHL => self.mem.read(self.reg.read_long(LongRegister::HL)),
            BitOperand::Register(reg) => self.reg.read(reg),
        };

        let mut carry_bit = val & 1;
        if include_carry && self.reg.get_flag(Flag::Carry) {
            carry_bit = 1;
        }

        let result = (val >> 1) | (carry_bit << 7);

        match operand {
            BitOperand::MemoryAtHL => self.mem.write(self.reg.read_long(LongRegister::HL), result),
            BitOperand::Register(reg) => self.reg.write(reg, result),
        };

        self.reg
            .set_flags(&[Flag::HalfCarry, Flag::Negative], false);
        self.reg.set_flag(Flag::Carry, carry_bit > 0);
        self.reg.set_flag(Flag::Zero, result == 0);
    }

    fn rotate_left(&mut self, operand: BitOperand, include_carry: bool) {
        let val = match operand {
            BitOperand::MemoryAtHL => self.mem.read(self.reg.read_long(LongRegister::HL)),
            BitOperand::Register(reg) => self.reg.read(reg),
        };

        let mut carry_bit = (val & 0b1000_0000) >> 7;
        if include_carry && self.reg.get_flag(Flag::Carry) {
            carry_bit = 1;
        }

        let result = (val << 1) | carry_bit;

        match operand {
            BitOperand::MemoryAtHL => self.mem.write(self.reg.read_long(LongRegister::HL), result),
            BitOperand::Register(reg) => self.reg.write(reg, result),
        };

        self.reg
            .set_flags(&[Flag::HalfCarry, Flag::Negative], false);
        self.reg.set_flag(Flag::Carry, carry_bit > 0);
        self.reg.set_flag(Flag::Zero, result == 0);
    }

    fn load(&mut self, dest: Destination, source: Source) {
        let val = self.get_source_value(source);

        match dest {
            Destination::Register(reg) => self.reg.write(reg, val),
            Destination::Memory(addr) => self.mem.write(addr, val),
            Destination::MemoryAtRegister(reg) => {
                let addr = self.reg.read_long(reg);
                self.mem.write(addr, val);
            }
        }
    }

    fn ld_a_mem(&mut self, addr: u16) {
        self.reg.write(Register::A, self.mem.read(addr));
    }

    fn load_high(&mut self, dest: LoadHighOperand, src: LoadHighOperand) {
        let val = match src {
            LoadHighOperand::MemoryAtNumber(n) => self.mem.read(0xFF00 & (n as u16)),
            LoadHighOperand::MemoryAtC => {
                self.mem.read(0xFF00 & (self.reg.read(Register::C) as u16))
            }
            LoadHighOperand::A => self.reg.read(Register::A),
        };

        match dest {
            LoadHighOperand::MemoryAtNumber(n) => self.mem.write(0xFF00 & (n as u16), val),
            LoadHighOperand::MemoryAtC => self
                .mem
                .write(0xFF00 & (self.reg.read(Register::C) as u16), val),
            LoadHighOperand::A => self.reg.write(Register::A, val),
        }
    }

    fn load_long(&mut self, dest: LongDestination, source: LongSource) {
        let val = match source {
            LongSource::Number(immediate) => immediate,
            LongSource::SP => self.sp,
        };

        match dest {
            LongDestination::Register(reg) => self.reg.write_long(reg, val),
            LongDestination::Memory(addr) => self.mem.write_u16(addr, val),
            LongDestination::SP => self.sp = val,
        }
    }

    fn incdec(&mut self, dest: IncdecDestination, op: IncrementOp) {
        let val = match dest {
            IncdecDestination::Register(reg) => self.reg.read(reg),
            IncdecDestination::MemoryAtHL => self.mem.read(self.reg.read_long(LongRegister::HL)),
        };

        let result = if op == IncrementOp::Inc {
            val.wrapping_add(1)
        } else {
            val.wrapping_sub(1)
        };

        self.reg.set_flag(Flag::Zero, result == 0);
        self.reg.set_flag(Flag::Zero, result == 0);
        self.reg.set_flag(Flag::Negative, op == IncrementOp::Dec);

        let halfcarry = if op == IncrementOp::Inc {
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

    fn incdec_long(&mut self, dest: IncdecLongDestination, op: IncrementOp) {
        let val = match dest {
            IncdecLongDestination::Register(reg) => self.reg.read_long(reg),
            IncdecLongDestination::SP => self.sp,
        };

        let result = if op == IncrementOp::Inc {
            val.wrapping_add(1)
        } else {
            val.wrapping_sub(1)
        };

        self.reg.set_flag(Flag::Zero, result == 0);
        self.reg.set_flag(Flag::Negative, op == IncrementOp::Dec);

        let halfcarry = if op == IncrementOp::Inc {
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

    fn add(&mut self, source: Source, add_with_carry: bool) {
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

    fn sub(&mut self, source: Source, sub_with_carry: bool) {
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

    fn and(&mut self, source: Source) {
        let val = self.get_source_value(source);

        let result = self.reg.read(Register::A) & val;
        self.reg.set_flag(Flag::Carry, false);
        self.reg.set_flag(Flag::Zero, result == 0);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, true);

        self.reg.write(Register::A, result);
    }

    fn xor(&mut self, source: Source) {
        let val = self.get_source_value(source);

        let result = self.reg.read(Register::A) ^ val;
        self.reg.set_flag(Flag::Carry, false);
        self.reg.set_flag(Flag::Zero, result == 0);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, false);

        self.reg.write(Register::A, result);
    }

    fn or(&mut self, source: Source) {
        let val = self.get_source_value(source);

        self.reg
            .write(Register::A, self.reg.read(Register::A) | val);
        self.reg.set_flag(Flag::Carry, false);
        self.reg
            .set_flag(Flag::Zero, self.reg.read(Register::A) == 0);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, false);
    }

    fn cp(&mut self, source: Source) {
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
    fn add_hl_hl(&mut self) {
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
        // TODO.MO: convenience function for this?
        let carry = if self.reg.get_flag(Flag::Carry) { 1 } else { 0 };

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
        // TODO.MO: convenience function for this?
        let carry = if self.reg.get_flag(Flag::Carry) { 1 } else { 0 };

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

    fn get_source_value(&self, source: Source) -> u8 {
        match source {
            Source::Register(reg) => self.reg.read(reg),
            Source::Number(x) => x,
            Source::MemoryAtRegister(reg) => self.mem.read(self.reg.read_long(reg)),
        }
    }

    // TODO: Move this out of the cpu
    fn parse_next_instr(&mut self) -> Result<Instruction, String> {
        use Instruction::*;
        use LongRegister::*;
        use Register::*;

        let opcode = self.mem.read(self.pc);

        Ok(match opcode {
            0x00 => NOP,
            0x10 => STOP,

            0x18 => JR(None, self.mem.read(self.pc + 1) as i8),
            0x28 => JR(Some(Condition::Zero), self.mem.read(self.pc + 1) as i8),
            0x38 => JR(Some(Condition::Carry), self.mem.read(self.pc + 1) as i8),
            0x20 => JR(Some(Condition::NotZero), self.mem.read(self.pc + 1) as i8),
            0x30 => JR(Some(Condition::NotCarry), self.mem.read(self.pc + 1) as i8),

            // Loads: 16 bit
            0x01 => LD_long(
                LongDestination::Register(BC),
                LongSource::Number(self.mem.read_u16(self.pc + 1)),
            ),
            0x11 => LD_long(
                LongDestination::Register(DE),
                LongSource::Number(self.mem.read_u16(self.pc + 1)),
            ),
            0x21 => LD_long(
                LongDestination::Register(HL),
                LongSource::Number(self.mem.read_u16(self.pc + 1)),
            ),
            0x31 => LD_long(
                LongDestination::SP,
                LongSource::Number(self.mem.read_u16(self.pc + 1)),
            ),
            0x08 => LD_long(
                LongDestination::Memory(self.mem.read_u16(self.pc + 1)),
                LongSource::SP,
            ),
            0xF8 => LD_hl_sp_n(self.mem.read(self.pc + 1)),
            0xF9 => LD_sp_hl,
            0xE8 => LD_long(
                LongDestination::SP,
                LongSource::Number(self.mem.read(self.pc + 1) as u16),
            ),

            // Loads: 8 bit
            0x02 => LD(Destination::MemoryAtRegister(BC), Source::Register(A)),
            0x12 => LD(Destination::MemoryAtRegister(DE), Source::Register(A)),
            0x22 => LD_incdec(
                Destination::MemoryAtRegister(HL),
                Source::Register(A),
                IncrementOp::Inc,
            ),
            0x32 => LD_incdec(
                Destination::MemoryAtRegister(HL),
                Source::Register(A),
                IncrementOp::Dec,
            ),
            0x06 => LD(
                Destination::Register(B),
                Source::Number(self.mem.read(self.pc + 1)),
            ),
            0x16 => LD(
                Destination::Register(D),
                Source::Number(self.mem.read(self.pc + 1)),
            ),
            0x26 => LD(
                Destination::Register(H),
                Source::Number(self.mem.read(self.pc + 1)),
            ),
            0x36 => LD(
                Destination::MemoryAtRegister(HL),
                Source::Number(self.mem.read(self.pc + 1)),
            ),
            0x0A => LD(Destination::Register(A), Source::MemoryAtRegister(BC)),
            0x1A => LD(Destination::Register(A), Source::MemoryAtRegister(DE)),
            0x2A => LD_incdec(
                Destination::Register(A),
                Source::MemoryAtRegister(HL),
                IncrementOp::Inc,
            ),
            0x3A => LD_incdec(
                Destination::Register(A),
                Source::MemoryAtRegister(HL),
                IncrementOp::Dec,
            ),
            0x0E => LD(
                Destination::Register(C),
                Source::Number(self.mem.read(self.pc + 1)),
            ),
            0x1E => LD(
                Destination::Register(E),
                Source::Number(self.mem.read(self.pc + 1)),
            ),
            0x2E => LD(
                Destination::Register(L),
                Source::Number(self.mem.read(self.pc + 1)),
            ),
            0x3E => LD(
                Destination::Register(A),
                Source::Number(self.mem.read(self.pc + 1)),
            ),

            0x46 => LD(Destination::Register(B), Source::MemoryAtRegister(HL)),
            0x4E => LD(Destination::Register(C), Source::MemoryAtRegister(HL)),

            0x56 => LD(Destination::Register(D), Source::MemoryAtRegister(HL)),
            0x5E => LD(Destination::Register(E), Source::MemoryAtRegister(HL)),

            0x66 => LD(Destination::Register(H), Source::MemoryAtRegister(HL)),
            0x6E => LD(Destination::Register(L), Source::MemoryAtRegister(HL)),

            0x7E => LD(Destination::Register(A), Source::MemoryAtRegister(HL)),

            x if (0x70..0x78).contains(&x) => LD(
                Destination::MemoryAtRegister(HL),
                Source::Register(get_second_reg_param(x).unwrap()),
            ),

            0xEA => LD(
                Destination::Memory(self.mem.read_u16(self.pc + 1)),
                Source::Register(A),
            ),
            0xFA => LD_a_mem(self.mem.read_u16(self.pc + 1)),
            0xE0 => LD_high(
                LoadHighOperand::MemoryAtNumber(self.mem.read(self.pc + 1)),
                LoadHighOperand::A,
            ),
            0xF0 => LD_high(
                LoadHighOperand::A,
                LoadHighOperand::MemoryAtNumber(self.mem.read(self.pc + 1)),
            ),
            0xE2 => LD_high(LoadHighOperand::MemoryAtC, LoadHighOperand::A),
            0xF2 => LD_high(LoadHighOperand::A, LoadHighOperand::MemoryAtC),

            0x76 => HALT,

            x if (0x40..0x80).contains(&x) => {
                let (dest, src) = get_reg_params(x).unwrap_or_else(|| {
                    panic!("This opcode doesn't take two registers as params: {x:X}")
                });

                LD(Destination::Register(dest), Source::Register(src))
            }

            x if (0x80..0x88).contains(&x) => ADD(get_arithmetic_second_param(x)),
            x if (0x88..0x8F).contains(&x) => ADC(get_arithmetic_second_param(x)),
            x if (0x90..0x98).contains(&x) => SUB(get_arithmetic_second_param(x)),
            x if (0x98..0x9F).contains(&x) => SBC(get_arithmetic_second_param(x)),
            x if (0xA0..0xA8).contains(&x) => AND(get_arithmetic_second_param(x)),
            x if (0xA8..0xAF).contains(&x) => XOR(get_arithmetic_second_param(x)),
            x if (0xB0..0xB8).contains(&x) => OR(get_arithmetic_second_param(x)),
            x if (0xB8..0xBF).contains(&x) => CP(get_arithmetic_second_param(x)),
            0xC6 => ADD(Source::Number(self.mem.read(self.pc + 1))),
            0xD6 => SUB(Source::Number(self.mem.read(self.pc + 1))),
            0xE6 => AND(Source::Number(self.mem.read(self.pc + 1))),
            0xF6 => OR(Source::Number(self.mem.read(self.pc + 1))),
            0xCE => ADC(Source::Number(self.mem.read(self.pc + 1))),
            0xDE => SBC(Source::Number(self.mem.read(self.pc + 1))),
            0xEE => XOR(Source::Number(self.mem.read(self.pc + 1))),
            0xFE => CP(Source::Number(self.mem.read(self.pc + 1))),

            // ADD 16 bit
            0x09 => ADD_hl(BC),
            0x19 => ADD_hl(DE),
            0x29 => ADD_hl(HL),
            0x39 => ADD_hl_hl,

            // INC/DEC: 16 bit
            0x03 => INCDEC_long(IncdecLongDestination::Register(BC), IncrementOp::Inc),
            0x13 => INCDEC_long(IncdecLongDestination::Register(DE), IncrementOp::Inc),
            0x23 => INCDEC_long(IncdecLongDestination::Register(HL), IncrementOp::Inc),
            0x33 => INCDEC_long(IncdecLongDestination::SP, IncrementOp::Inc),

            0x0B => INCDEC_long(IncdecLongDestination::Register(BC), IncrementOp::Dec),
            0x1B => INCDEC_long(IncdecLongDestination::Register(DE), IncrementOp::Dec),
            0x2B => INCDEC_long(IncdecLongDestination::Register(HL), IncrementOp::Dec),
            0x3B => INCDEC_long(IncdecLongDestination::SP, IncrementOp::Dec),

            // INC/DEC: 8 bit
            0x04 => INCDEC(IncdecDestination::Register(B), IncrementOp::Inc),
            0x14 => INCDEC(IncdecDestination::Register(D), IncrementOp::Inc),
            0x24 => INCDEC(IncdecDestination::Register(H), IncrementOp::Inc),
            0x34 => INCDEC(IncdecDestination::MemoryAtHL, IncrementOp::Inc),
            0x0C => INCDEC(IncdecDestination::Register(C), IncrementOp::Inc),
            0x1C => INCDEC(IncdecDestination::Register(E), IncrementOp::Inc),
            0x2C => INCDEC(IncdecDestination::Register(L), IncrementOp::Inc),
            0x3C => INCDEC(IncdecDestination::Register(A), IncrementOp::Inc),

            0x05 => INCDEC(IncdecDestination::Register(B), IncrementOp::Dec),
            0x15 => INCDEC(IncdecDestination::Register(D), IncrementOp::Dec),
            0x25 => INCDEC(IncdecDestination::Register(H), IncrementOp::Dec),
            0x35 => INCDEC(IncdecDestination::MemoryAtHL, IncrementOp::Dec),
            0x0D => INCDEC(IncdecDestination::Register(C), IncrementOp::Dec),
            0x1D => INCDEC(IncdecDestination::Register(E), IncrementOp::Dec),
            0x2D => INCDEC(IncdecDestination::Register(L), IncrementOp::Dec),
            0x3D => INCDEC(IncdecDestination::Register(A), IncrementOp::Dec),

            // A rotates
            0x07 => RLCA,
            0x17 => RLA,
            0x0F => RRCA,
            0x1F => RRA,

            0xC5 => PUSH(BC),
            0xD5 => PUSH(DE),
            0xE5 => PUSH(HL),
            0xF5 => PUSH(AF),

            // Jumps
            0xC3 => JP(None, self.mem.read_u16(self.pc + 1)),
            0xE9 => JP_hl,

            0xC7 => RST(0),
            0xD7 => RST(0x10),
            0xE7 => RST(0x20),
            0xF7 => RST(0x30),
            0xCF => RST(0x8),
            0xDF => RST(0x18),
            0xEF => RST(0x28),
            0xFF => RST(0x38),

            0xCD => CALL(None, self.mem.read_u16(self.pc + 1)),
            0xC4 => CALL(Some(Condition::NotZero), self.mem.read_u16(self.pc + 1)),
            0xD4 => CALL(Some(Condition::NotCarry), self.mem.read_u16(self.pc + 1)),
            0xCC => CALL(Some(Condition::Zero), self.mem.read_u16(self.pc + 1)),
            0xDC => CALL(Some(Condition::Carry), self.mem.read_u16(self.pc + 1)),

            0xC9 => RET(None),
            0xC0 => RET(Some(Condition::NotZero)),
            0xD0 => RET(Some(Condition::NotCarry)),
            0xC8 => RET(Some(Condition::Zero)),
            0xD8 => RET(Some(Condition::Carry)),

            0xF3 => DI,
            0xFB => EI,

            0xCB => self.parse_bit_instr()?,

            // TODO: This is probably wrong? They seem to take an offset, dunno
            // 0x20 => JP(Some(Condition::NotZero), self.mem.read_u16(self.pc + 1)),
            // 0x30 => JP(Some(Condition::NotCarry), self.mem.read_u16(self.pc + 1)),
            invalid => return Err(format!("Invalid opcode: 0x{invalid:X}")),
        })
    }

    fn parse_bit_instr(&mut self) -> Result<Instruction, String> {
        use Instruction::*;
        let opcode = self.mem.read(self.pc + 1);

        Ok(match opcode {
            x if (0x00..0x08).contains(&x) => RLC(get_bit_param(x)),
            x if (0x08..0x0F).contains(&x) => RRC(get_bit_param(x)),
            x if (0x10..0x18).contains(&x) => RL(get_bit_param(x)),
            x if (0x18..0x1F).contains(&x) => RR(get_bit_param(x)),
            x if (0x20..0x28).contains(&x) => SLA(get_bit_param(x)),
            x if (0x28..0x2F).contains(&x) => SRA(get_bit_param(x)),
            x if (0x30..0x38).contains(&x) => SWAP(get_bit_param(x)),
            x if (0x38..0x3F).contains(&x) => SRL(get_bit_param(x)),

            x if (0x40..0x48).contains(&x) => BIT(0, get_bit_param(x)),
            x if (0x48..0x4F).contains(&x) => BIT(1, get_bit_param(x)),
            x if (0x50..0x58).contains(&x) => BIT(2, get_bit_param(x)),
            x if (0x58..0x5F).contains(&x) => BIT(3, get_bit_param(x)),
            x if (0x60..0x68).contains(&x) => BIT(4, get_bit_param(x)),
            x if (0x68..0x6F).contains(&x) => BIT(5, get_bit_param(x)),
            x if (0x70..0x78).contains(&x) => BIT(6, get_bit_param(x)),
            x if (0x78..0x7F).contains(&x) => BIT(7, get_bit_param(x)),

            x if (0x80..0x88).contains(&x) => RES(0, get_bit_param(x)),
            x if (0x88..0x8F).contains(&x) => RES(1, get_bit_param(x)),
            x if (0x90..0x98).contains(&x) => RES(2, get_bit_param(x)),
            x if (0x98..0x9F).contains(&x) => RES(3, get_bit_param(x)),
            x if (0xA0..0xA8).contains(&x) => RES(4, get_bit_param(x)),
            x if (0xA8..0xAF).contains(&x) => RES(5, get_bit_param(x)),
            x if (0xB0..0xB8).contains(&x) => RES(6, get_bit_param(x)),
            x if (0xB8..0xBF).contains(&x) => RES(7, get_bit_param(x)),

            x if (0xC0..0xC8).contains(&x) => SET(0, get_bit_param(x)),
            x if (0xC8..0xCF).contains(&x) => SET(1, get_bit_param(x)),
            x if (0xD0..0xD8).contains(&x) => SET(2, get_bit_param(x)),
            x if (0xD8..0xDF).contains(&x) => SET(3, get_bit_param(x)),
            x if (0xE0..0xE8).contains(&x) => SET(4, get_bit_param(x)),
            x if (0xE8..0xEF).contains(&x) => SET(5, get_bit_param(x)),
            x if (0xF0..0xF8).contains(&x) => SET(6, get_bit_param(x)),
            x if (0xF8..0xFF).contains(&x) => SET(7, get_bit_param(x)),

            invalid => return Err(format!("Invalid bit operation opcode: 0x{invalid:X}")),
        })
    }

    fn ld_sp_hl(&mut self) {
        self.sp = self.reg.read_long(LongRegister::HL);
    }

    fn ld_hl_sp_n(&mut self, offset: u8) {
        let val = self.mem.read_u16(self.sp + offset as u16);
        self.reg.write_long(LongRegister::HL, val);
    }

    fn cpl(&mut self) {
        let val = self.reg.read(Register::A);
        self.reg.write(Register::A, !val);
        self.reg.set_flags(&[Flag::HalfCarry, Flag::Negative], true);
    }

    fn ccf(&mut self) {
        self.reg
            .set_flag(Flag::Carry, self.reg.get_flag(Flag::Carry));
        self.reg.set_flags(&[Flag::HalfCarry, Flag::Carry], false);
    }

    fn scf(&mut self) {
        self.reg.set_flag(Flag::Carry, true);
        self.reg.set_flags(&[Flag::HalfCarry, Flag::Carry], false);
    }

    fn jump(&mut self, cond: Option<Condition>, addr: u16) {
        let should_jump = match cond {
            None => true,
            Some(Condition::Zero) => self.reg.get_flag(Flag::Zero),
            Some(Condition::Carry) => self.reg.get_flag(Flag::Carry),
            Some(Condition::NotZero) => !self.reg.get_flag(Flag::Zero),
            Some(Condition::NotCarry) => !self.reg.get_flag(Flag::Carry),
        };

        if should_jump {
            self.pc = addr;
        }
    }

    fn relative_jump(&mut self, cond: Option<Condition>, offset: i8) {
        let should_jump = match cond {
            None => true,
            Some(Condition::Zero) => self.reg.get_flag(Flag::Zero),
            Some(Condition::Carry) => self.reg.get_flag(Flag::Carry),
            Some(Condition::NotZero) => !self.reg.get_flag(Flag::Zero),
            Some(Condition::NotCarry) => !self.reg.get_flag(Flag::Carry),
        };

        if should_jump {
            self.pc = self.pc.checked_add_signed(offset as i16).unwrap();
        }
    }

    fn call(&mut self, cond: Option<Condition>, addr: u16) {
        let should_call = match cond {
            None => true,
            Some(Condition::Zero) => self.reg.get_flag(Flag::Zero),
            Some(Condition::Carry) => self.reg.get_flag(Flag::Carry),
            Some(Condition::NotZero) => !self.reg.get_flag(Flag::Zero),
            Some(Condition::NotCarry) => !self.reg.get_flag(Flag::Carry),
        };

        if should_call {
            self.push_pc_stack();
            self.pc = addr;
        }
    }

    fn ret(&mut self, cond: Option<Condition>) {
        let should_return = match cond {
            None => true,
            Some(Condition::Zero) => self.reg.get_flag(Flag::Zero),
            Some(Condition::Carry) => self.reg.get_flag(Flag::Carry),
            Some(Condition::NotZero) => !self.reg.get_flag(Flag::Zero),
            Some(Condition::NotCarry) => !self.reg.get_flag(Flag::Carry),
        };

        if should_return {
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
// TODO: Don't return a Source here; instead return a narrower type that is only
// Register|MemoryAtRegister(LongRegister::HL) and impement from::From<NarrowerType> for Source
// After this is done, get_bit_param can be merged with this function.
fn get_arithmetic_second_param(opcode: u8) -> Source {
    use Register::*;

    match (opcode & 0x0F) % 8 {
        0 => Source::Register(B),
        1 => Source::Register(C),
        2 => Source::Register(D),
        3 => Source::Register(E),
        4 => Source::Register(H),
        5 => Source::Register(L),
        6 => Source::MemoryAtRegister(LongRegister::HL),
        7 => Source::Register(A),
        x => panic!("This should never happen: we're looking at the second nibble only with modulo 8, value was: 0x{x:X}")
    }
}

// TODO: Naming, this is also used for bit operations where it's not the -second- param necessarily
fn get_bit_param(opcode: u8) -> BitOperand {
    use Register::*;

    match (opcode & 0x0F) % 8 {
        0 => BitOperand::Register(B),
        1 => BitOperand::Register(C),
        2 => BitOperand::Register(D),
        3 => BitOperand::Register(E),
        4 => BitOperand::Register(H),
        5 => BitOperand::Register(L),
        6 => BitOperand::MemoryAtHL,
        7 => BitOperand::Register(A),
        x => panic!("This should never happen: we're looking at the second nibble only with modulo 8, value was: 0x{x:X}")
    }
}

fn get_reg_params(opcode: u8) -> Option<(Register, Register)> {
    use Register::*;

    let high = opcode & 0xF0;
    let low = opcode & 0x0F;

    // TODO: low matching seems unnecessary?
    Some(match (high, low) {
        (40, low) if low < 8 => (B, get_second_reg_param(low)?),
        (40, low) => (C, get_second_reg_param(low)?),

        (50, low) if low < 8 => (D, get_second_reg_param(low)?),
        (50, low) => (E, get_second_reg_param(low)?),

        (60, low) if low < 8 => (H, get_second_reg_param(low)?),
        (60, low) => (L, get_second_reg_param(low)?),

        (70, low) if low > 7 => (A, get_second_reg_param(low)?),

        _ => return None,
    })
}

fn get_second_reg_param(number: u8) -> Option<Register> {
    use Register::*;

    Some(match number % 8 {
        0 => B,
        1 => C,
        2 => D,
        3 => E,
        4 => H,
        5 => L,
        7 => A,
        _ => return None,
    })
}
