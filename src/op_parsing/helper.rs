use crate::{
    instruction::{AluSource, BitOperand, LoadDestination, LoadSource},
    registers::Register,
};

#[derive(Copy, Clone)]
pub enum RegOrMemHl {
    MemoryAtHl,
    Register(Register),
}

impl From<RegOrMemHl> for AluSource {
    fn from(value: RegOrMemHl) -> Self {
        match value {
            RegOrMemHl::MemoryAtHl => AluSource::MemoryAtHl,
            RegOrMemHl::Register(reg) => AluSource::Register(reg),
        }
    }
}
impl From<RegOrMemHl> for BitOperand {
    fn from(value: RegOrMemHl) -> Self {
        match value {
            RegOrMemHl::MemoryAtHl => BitOperand::MemoryAtHl,
            RegOrMemHl::Register(reg) => BitOperand::Register(reg),
        }
    }
}

impl From<RegOrMemHl> for LoadDestination {
    fn from(value: RegOrMemHl) -> Self {
        match value {
            RegOrMemHl::MemoryAtHl => LoadDestination::MemoryAtHl(None),
            RegOrMemHl::Register(reg) => LoadDestination::Register(reg),
        }
    }
}

impl From<RegOrMemHl> for LoadSource {
    fn from(value: RegOrMemHl) -> Self {
        match value {
            RegOrMemHl::MemoryAtHl => LoadSource::MemoryAtHl(None),
            RegOrMemHl::Register(reg) => LoadSource::Register(reg),
        }
    }
}
