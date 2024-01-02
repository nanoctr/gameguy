use crate::registers::{LongRegister, Register};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Source {
    Register(Register),
    Number(u8),
    MemoryAtRegister(LongRegister),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Destination {
    Register(Register),
    Memory(u16),
    MemoryAtRegister(LongRegister),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LongSource {
    Number(u16),
    SP,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LongDestination {
    Register(LongRegister),
    Memory(u16),
    SP,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IncdecDestination {
    Register(Register),
    MemoryAtHL,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IncdecLongDestination {
    Register(LongRegister),
    SP,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IncrementOp {
    Inc,
    Dec,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LoadHighOperand {
    MemoryAtNumber(u8),
    MemoryAtC,
    A,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Condition {
    Zero,
    Carry,
    NotZero,
    NotCarry,
}

#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    NOP,
    STOP,
    HALT,
    LD(Destination, Source),
    LD_long(LongDestination, LongSource),
    LD_incdec(Destination, Source, IncrementOp),
    INCDEC(IncdecDestination, IncrementOp),
    INCDEC_long(IncdecLongDestination, IncrementOp),
    LD_hl_sp_n(u8),
    LD_sp_hl,
    LD_a_mem(u16),
    LD_high(LoadHighOperand, LoadHighOperand),

    ADD(Source),
    ADC(Source),
    SUB(Source),
    SBC(Source),
    AND(Source),
    XOR(Source),
    OR(Source),
    CP(Source),

    ADD_hl(LongRegister),
    ADD_hl_hl,

    RLCA,
    RLA,
    RRCA,
    RRA,

    CPL,

    CCF,
    SCF,

    PUSH(LongRegister),
    POP(LongRegister),

    JP(Option<Condition>, u16),
    JP_hl,
    JR(Option<Condition>, i8),

    RST(u8),
    CALL(Option<Condition>, u16),
    RET(Option<Condition>),

    // Bit operations
    RLC(BitOperand),
    RRC(BitOperand),
    RL(BitOperand),
    RR(BitOperand),
    SLA(BitOperand),
    SRA(BitOperand),
    SWAP(BitOperand),
    SRL(BitOperand),

    BIT(u8, BitOperand),
    RES(u8, BitOperand),
    SET(u8, BitOperand),

    // interrupts
    EI,
    DI,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BitOperand {
    MemoryAtHL,
    Register(Register),
}

impl Instruction {
    pub fn length(&self) -> Result<u16, String> {
        use Instruction::*;

        Ok(match self {
            NOP | STOP | HALT => 1,
            LD_long(_, LongSource::Number(_)) => 3,
            LD_long(_, LongSource::SP) => 3,
            LD_sp_hl => 1,
            LD_hl_sp_n(_) => 2,
            LD(Destination::Memory(_), Source::Number(_)) => 3,
            LD(Destination::MemoryAtRegister(_), Source::Number(_)) => 3,
            LD(_, Source::Number(_)) => 2,
            LD(_, Source::Register(_)) => 1,
            LD_a_mem(_) => 3,
            LD_high(_, _) => 2,
            LD_incdec(_, _, _) => 1,
            INCDEC(_, _) => 1,
            INCDEC_long(_, _) => 1,
            JP(_, _) => 3,
            JP_hl => 1,
            JR(_, _) => 2,
            PUSH(_) => 1,
            POP(_) => 1,

            ADD(_) | ADC(_) | SUB(_) | SBC(_) | AND(_) | XOR(_) | OR(_) | CP(_) => 1,
            ADD_hl(_) | ADD_hl_hl => 1,

            RLA | RLCA | RRA | RRCA => 1,

            CPL => 1,

            CCF => 1,
            SCF => 1,

            RST(_) => 1,
            CALL(_, _) => 3,
            RET(_) => 1,

            RLC(_) | RRC(_) | RL(_) | RR(_) | SLA(_) | SRA(_) | SWAP(_) | SRL(_) => 2,

            BIT(_, _) | RES(_, _) | SET(_, _) => 1,

            EI | DI => 1,
            // TODO: This feels icky :/
            _ => todo!(),
        })
    }

    pub fn cycles(&self) -> Result<u16, String> {
        use Instruction::*;

        Ok(match self {
            NOP | STOP | HALT => 4,
            LD_long(LongDestination::Register(_), LongSource::Number(_)) => 12,
            LD_long(LongDestination::Memory(_), LongSource::SP) => 20,
            LD_sp_hl => 8,
            LD_hl_sp_n(_) => 12,
            LD(Destination::MemoryAtRegister(_), Source::Register(_)) => 8,
            LD(Destination::Register(_), Source::Number(_)) => 8,
            LD(Destination::Register(_), Source::MemoryAtRegister(_)) => 8,
            LD(Destination::Register(_), Source::Register(_)) => 4,
            LD(Destination::Memory(_), Source::Register(_)) => 16,
            LD_a_mem(_) => 16,
            LD_high(LoadHighOperand::MemoryAtNumber(_), _)
            | LD_high(_, LoadHighOperand::MemoryAtNumber(_)) => 12,
            LD_high(_, _) => 8,
            LD_incdec(_, _, _) => 8,
            INCDEC(IncdecDestination::Register(_), _) => 4,
            INCDEC(IncdecDestination::MemoryAtHL, _) => 12,
            INCDEC_long(IncdecLongDestination::Register(_), _) => 4,
            INCDEC_long(IncdecLongDestination::SP, _) => 12,

            ADD(Source::MemoryAtRegister(_)) => 8,
            ADD(Source::Register(_)) => 4,
            ADC(Source::MemoryAtRegister(_)) => 8,
            ADC(Source::Register(_)) => 4,
            SUB(Source::MemoryAtRegister(_)) => 8,
            SUB(Source::Register(_)) => 4,
            SBC(Source::MemoryAtRegister(_)) => 8,
            SBC(Source::Register(_)) => 4,
            AND(Source::MemoryAtRegister(_)) => 8,
            AND(Source::Register(_)) => 4,
            XOR(Source::MemoryAtRegister(_)) => 8,
            XOR(Source::Register(_)) => 4,
            OR(Source::MemoryAtRegister(_)) => 8,
            OR(Source::Register(_)) => 4,
            CP(Source::MemoryAtRegister(_)) => 8,
            CP(Source::Register(_)) => 4,

            ADD_hl(_) | ADD_hl_hl => 8,

            JP(_, _) => 16, // TODO: ?
            JP_hl => 4,
            JR(_, _) => 12, // TODO: ?
            PUSH(_) => 16,
            POP(_) => 12,

            RLCA | RLA | RRA | RRCA => 4,
            CPL => 4,

            CCF => 4,
            SCF => 4,

            RST(_) => 16,
            CALL(_, _) => 24, // TODO: ?
            RET(_) => 20,     // TODO: ?

            RLC(BitOperand::MemoryAtHL)
            | RRC(BitOperand::MemoryAtHL)
            | RL(BitOperand::MemoryAtHL)
            | RR(BitOperand::MemoryAtHL)
            | SLA(BitOperand::MemoryAtHL)
            | SRA(BitOperand::MemoryAtHL)
            | SWAP(BitOperand::MemoryAtHL)
            | SRL(BitOperand::MemoryAtHL) => 16,

            RLC(_) | RRC(_) | RL(_) | RR(_) | SLA(_) | SRA(_) | SWAP(_) | SRL(_) => 8,

            BIT(_, BitOperand::MemoryAtHL)
            | RES(_, BitOperand::MemoryAtHL)
            | SET(_, BitOperand::MemoryAtHL) => 12,

            BIT(_, _) | RES(_, _) | SET(_, _) => 8,

            EI | DI => 4,

            _ => return Err(format!("Invalid instruction: {self:?}")),
        })
    }

    // TODO: This doesn't work with conditional jumps
    pub fn is_jump(&self) -> bool {
        todo!("This is garbage.");
    }
}
