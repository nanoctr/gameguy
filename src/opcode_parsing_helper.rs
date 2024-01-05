// TODO: This should probably also contain the parsing logic itself somehow
// TODO: Also, these types should be private

use crate::{
    instruction::{ArithmeticSource, BitOperand, LoadDestination, LoadSource},
    registers::Register,
};

#[derive(Copy, Clone)]
pub enum RegOrMemHl {
    MemoryAtHl,
    Register(Register),
}

impl From<RegOrMemHl> for ArithmeticSource {
    fn from(value: RegOrMemHl) -> Self {
        match value {
            RegOrMemHl::MemoryAtHl => ArithmeticSource::MemoryAtHl,
            RegOrMemHl::Register(reg) => ArithmeticSource::Register(reg),
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
