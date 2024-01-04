enum Instrunction {
    LD(Destination, Source),
    LD_long(Destination, Source),
}

enum LongRegister {}
enum Register {}

enum HighMemoryOperand {
    Immediate(u8),
    A,
    C,
}

enum LoadDestination {
    Register(Register),
    MemoryAtHl,
    MemoryAt(u16),
    ZeroMemory(u8),
    MemoryAtC,
    HighMemory(HighMemoryOperand),
}

enum IncDecOp {
    Inc,
    Dec,
}

enum LoadSource {
    Immediate(u8),
    MemoryAtRegister(LongRegister),
    MemoryAtHl(Option<IncDecOp>),
    Register,
    ZeroMemory,
    MemoryAtC,
    MemoryAt(u16),
    HighMemory(HighMemoryOperand),
}

enum LoadLongDestination {
    Register(LongRegister),
    SP,
    MemoryAt(u16),
}

enum LoadLongSource {
    Immediate(u16),
    SP,
    SP_Offset(i8),
    HL,
}
