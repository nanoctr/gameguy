use crate::registers::{LongRegister, Register};

#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Instruction {
    NOP,
    STOP,
    HALT,

    // loads
    LD(LoadDestination, LoadSource),
    LD_long(LoadLongDestination, LoadLongSource),

    // arithmetic operations
    ADD(AluSource),
    ADC(AluSource),
    SUB(AluSource),
    SBC(AluSource),
    AND(AluSource),
    XOR(AluSource),
    OR(AluSource),
    CP(AluSource),

    ADD_hl(LongRegister),
    ADD_hl_sp,

    INCDEC(IncdecDestination, IncDecOp),
    INCDEC_long(IncdecLongDestination, IncDecOp),

    // Bit operations
    RLCA,
    RLA,
    RRCA,
    RRA,

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

    // jumps and calls
    JP(Option<Condition>, u16),
    JP_hl,
    JR(Option<Condition>, i8),

    RST(u8),
    CALL(Option<Condition>, u16),
    RET(Option<Condition>),

    // interrupts
    EI,
    DI,

    // misc
    PUSH(LongRegister),
    POP(LongRegister),

    CPL,
    SCF,
    CCF,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AluSource {
    Register(Register),
    Immediate(u8),
    MemoryAtHl,
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
pub enum IncDecOp {
    Inc,
    Dec,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Condition {
    Zero,
    Carry,
    NotZero,
    NotCarry,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IoMemoryOffset {
    Immediate(u8),
    C,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LoadDestination {
    Register(Register),
    MemoryAtRegister(LongRegister),
    MemoryAtHl(Option<IncDecOp>),
    MemoryAt(u16),
    IoMemory(IoMemoryOffset),
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LoadSource {
    Immediate(u8),
    MemoryAtRegister(LongRegister),
    MemoryAtHl(Option<IncDecOp>),
    Register(Register),
    MemoryAt(u16),
    IoMemory(IoMemoryOffset),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LoadLongDestination {
    Register(LongRegister),
    SP,
    MemoryAt(u16),
}

#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LoadLongSource {
    Immediate(u16),
    SP,
    SP_Offset(i8),
    HL,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BitOperand {
    MemoryAtHl,
    Register(Register),
}
