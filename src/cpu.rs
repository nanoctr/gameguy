use crate::{
    instruction::{
        AluSource, BitOperand, Condition, IncDecOp, IncdecDestination, IncdecLongDestination,
        Instruction, IoMemoryOffset, LoadDestination, LoadLongDestination, LoadLongSource,
        LoadSource,
    },
    mmu::Mmu,
    op_parsing,
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
        let instr = op_parsing::parse_next_instr(&self.mem, self.pc);
        dbg!(instr);
        self.execute(&instr);
    }

    fn execute(&mut self, instr: &Instruction) {
        use Instruction::*;

        match *instr {
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

            RLCA => self.rotate_left(BitOperand::Register(Register::A), false),
            RLA => self.rotate_left(BitOperand::Register(Register::A), true),
            RRCA => self.rotate_right(BitOperand::Register(Register::A), false),
            RRA => self.rotate_right(BitOperand::Register(Register::A), true),

            RLC(operand) => self.rotate_left(operand, false),
            RRC(operand) => self.rotate_right(operand, false),
            RL(operand) => self.rotate_left(operand, true),
            RR(operand) => self.rotate_right(operand, true),
            SLA(operand) => self.shift_left(operand, false),
            SRL(operand) => self.shift_right(operand, false),
            SRA(operand) => self.shift_right_arithmetic(operand),
            SWAP(operand) => self.swap(operand),

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
        };

        if let Some(inc) = pc_incr(instr) {
            self.pc += inc;
        }
    }

    // loads

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

    // arithmetic operations

    fn add(&mut self, source: AluSource, add_with_carry: bool) {
        let carry = u8::from(add_with_carry && self.reg.get_flag(Flag::Carry));
        let val = self.read_alu_source(source);

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

    fn sub(&mut self, source: AluSource, sub_with_carry: bool) {
        let carry = u8::from(sub_with_carry && self.reg.get_flag(Flag::Carry));
        let val = self.read_alu_source(source);

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

    fn and(&mut self, source: AluSource) {
        let val = self.read_alu_source(source);

        let result = self.reg.read(Register::A) & val;
        self.reg.set_flag(Flag::Carry, false);
        self.reg.set_flag(Flag::Zero, result == 0);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, true);

        self.reg.write(Register::A, result);
    }

    fn xor(&mut self, source: AluSource) {
        let val = self.read_alu_source(source);

        let result = self.reg.read(Register::A) ^ val;
        self.reg.set_flag(Flag::Carry, false);
        self.reg.set_flag(Flag::Zero, result == 0);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, false);

        self.reg.write(Register::A, result);
    }

    fn or(&mut self, source: AluSource) {
        let val = self.read_alu_source(source);

        self.reg
            .write(Register::A, self.reg.read(Register::A) | val);
        self.reg.set_flag(Flag::Carry, false);
        self.reg
            .set_flag(Flag::Zero, self.reg.read(Register::A) == 0);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, false);
    }

    fn cp(&mut self, source: AluSource) {
        let val = self.read_alu_source(source);

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
                self.mem.write(self.reg.read_long(LongRegister::HL), result);
            }
        };
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
        };
    }

    // bit operations

    fn rotate_left(&mut self, operand: BitOperand, through_carry_flag: bool) {
        let val = self.read_bit_operand(operand);

        let carry = if through_carry_flag {
            u8::from(self.reg.get_flag(Flag::Carry))
        } else {
            (val & 0b1000_0000) >> 7
        };

        let result = (val << 1) | carry;

        self.write_bit_operand(operand, result);

        self.reg
            .set_flags(&[Flag::HalfCarry, Flag::Negative], false);
        self.reg.set_flag(Flag::Carry, carry > 0);
        self.reg.set_flag(Flag::Zero, result == 0);
    }

    fn rotate_right(&mut self, operand: BitOperand, through_carry_flag: bool) {
        let val = self.read_bit_operand(operand);

        let carry = if through_carry_flag {
            u8::from(self.reg.get_flag(Flag::Carry))
        } else {
            val & 1
        };

        let result = (val >> 1) | (carry << 7);

        self.write_bit_operand(operand, result);

        self.reg
            .set_flags(&[Flag::HalfCarry, Flag::Negative], false);
        self.reg.set_flag(Flag::Carry, carry > 0);
        self.reg.set_flag(Flag::Zero, result == 0);
    }

    fn shift_left(&mut self, operand: BitOperand, through_carry_flag: bool) {
        let val = self.read_bit_operand(operand);

        let carry = if through_carry_flag {
            u8::from(self.reg.get_flag(Flag::Carry))
        } else {
            (val & 0b1000_0000) >> 7
        };

        let result = val << 1;

        self.write_bit_operand(operand, result);

        self.reg
            .set_flags(&[Flag::HalfCarry, Flag::Negative], false);
        self.reg.set_flag(Flag::Carry, carry > 0);
        self.reg.set_flag(Flag::Zero, result == 0);
    }

    fn shift_right(&mut self, operand: BitOperand, through_carry_flag: bool) {
        let val = self.read_bit_operand(operand);

        let carry = if through_carry_flag {
            u8::from(self.reg.get_flag(Flag::Carry))
        } else {
            val & 1
        };

        let result = val >> 1;

        self.write_bit_operand(operand, result);

        self.reg
            .set_flags(&[Flag::HalfCarry, Flag::Negative], false);
        self.reg.set_flag(Flag::Carry, carry > 0);
        self.reg.set_flag(Flag::Zero, result == 0);
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

    // jumps

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
            self.mem.write(self.sp - 1, (self.pc >> 8) as u8);
            self.mem.write(self.sp - 2, self.pc as u8);
            self.sp -= 2;
            self.pc = addr;
        }
    }

    fn ret(&mut self, cond: Option<Condition>) {
        if cond.is_none() || self.eval_condition(cond.unwrap()) {
            let new_pc = (self.mem.read(self.sp + 1) as u16) << 8 | (self.mem.read(self.sp) as u16);
            self.sp += 2;
            self.pc = new_pc;
        }
    }

    // misc

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

    fn cpl(&mut self) {
        self.reg.write(Register::A, !self.reg.read(Register::A));
        self.reg.set_flags(&[Flag::HalfCarry, Flag::Negative], true);
    }

    fn scf(&mut self) {
        self.reg.set_flag(Flag::Carry, true);
        self.reg.set_flags(&[Flag::HalfCarry, Flag::Carry], false);
    }

    fn ccf(&mut self) {
        self.reg
            .set_flag(Flag::Carry, !self.reg.get_flag(Flag::Carry));
        self.reg.set_flags(&[Flag::HalfCarry, Flag::Carry], false);
    }

    // helpers

    fn eval_condition(&self, cond: Condition) -> bool {
        match cond {
            Condition::Zero => self.reg.get_flag(Flag::Zero),
            Condition::Carry => self.reg.get_flag(Flag::Carry),
            Condition::NotZero => !self.reg.get_flag(Flag::Zero),
            Condition::NotCarry => !self.reg.get_flag(Flag::Carry),
        }
    }

    fn read_alu_source(&self, source: AluSource) -> u8 {
        match source {
            AluSource::Register(reg) => self.reg.read(reg),
            AluSource::Immediate(x) => x,
            AluSource::MemoryAtHl => self.mem.read(self.reg.read_long(LongRegister::HL)),
        }
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
}

fn pc_incr(instr: &Instruction) -> Option<u16> {
    use Instruction::*;

    match instr {
        NOP => Some(1),
        STOP => None,
        HALT => None,
        LD(_, _) => Some(1),
        LD_long(_, _) => Some(2),
        ADD(_) => Some(1),
        ADC(_) => Some(1),
        SUB(_) => Some(1),
        SBC(_) => Some(1),
        AND(_) => Some(1),
        XOR(_) => Some(1),
        OR(_) => Some(1),
        CP(_) => Some(1),
        ADD_hl(_) => Some(1),
        ADD_hl_sp => Some(1),
        INCDEC(_, _) => Some(1),
        INCDEC_long(_, _) => Some(1),
        RLCA => Some(1),
        RLA => Some(2),
        RRCA => Some(1),
        RRA => Some(2),
        RLC(_) => Some(2),
        RRC(_) => Some(2),
        RL(_) => Some(2),
        RR(_) => Some(2),
        SLA(_) => Some(2),
        SRA(_) => Some(2),
        SWAP(_) => Some(2),
        SRL(_) => Some(2),
        BIT(_, _) => Some(2),
        RES(_, _) => Some(2),
        SET(_, _) => Some(2),
        JP(_, _) => None,
        JP_hl => None,
        JR(_, _) => None,
        RST(_) => None,
        CALL(_, _) => None,
        RET(_) => None,
        EI => Some(1),
        DI => Some(1),
        PUSH(_) => Some(1),
        POP(_) => Some(1),
        CPL => Some(1),
        SCF => Some(1),
        CCF => Some(1),
    }
}
