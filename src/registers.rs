pub struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    f: u8,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Register {
    A,
    B,
    C,
    D,
    E,
    F,
    H,
    L,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LongRegister {
    AF,
    BC,
    DE,
    HL,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Flag {
    Zero,
    Negative,
    HalfCarry,
    Carry,
}

impl Flag {
    fn get_flag_bit_offset(self) -> u8 {
        match self {
            Flag::Zero => 7,
            Flag::Negative => 6,
            Flag::HalfCarry => 5,
            Flag::Carry => 4,
        }
    }
}

impl Registers {
    pub fn read(&self, reg: Register) -> u8 {
        match reg {
            Register::A => self.a,
            Register::B => self.b,
            Register::C => self.c,
            Register::D => self.d,
            Register::E => self.e,
            Register::F => self.f & 0xF0,
            Register::H => self.h,
            Register::L => self.l,
        }
    }

    pub fn write(&mut self, reg: Register, val: u8) {
        match reg {
            Register::A => self.a = val,
            Register::B => self.b = val,
            Register::C => self.c = val,
            Register::D => self.d = val,
            Register::E => self.e = val,
            Register::F => self.f = val & 0xF0,
            Register::H => self.h = val,
            Register::L => self.l = val,
        }
    }

    pub fn read_long(&self, reg: LongRegister) -> u16 {
        match reg {
            LongRegister::AF => (self.a as u16) << 8 | (self.f as u16),
            LongRegister::BC => (self.b as u16) << 8 | (self.c as u16),
            LongRegister::DE => (self.d as u16) << 8 | (self.e as u16),
            LongRegister::HL => (self.h as u16) << 8 | (self.l as u16),
        }
    }

    pub fn write_long(&mut self, reg: LongRegister, val: u16) {
        match reg {
            LongRegister::AF => {
                self.a = (val >> 8) as u8;
                self.f = val as u8;
            }
            LongRegister::BC => {
                self.a = (val >> 8) as u8;
                self.f = val as u8;
            }
            LongRegister::DE => {
                self.a = (val >> 8) as u8;
                self.f = val as u8;
            }
            LongRegister::HL => {
                self.a = (val >> 8) as u8;
                self.f = val as u8;
            }
        }
    }

    pub fn set_flag(&mut self, flag: Flag, value: bool) {
        let flag_bit_offset = flag.get_flag_bit_offset();

        if value {
            self.f |= 0b0000_0001 << flag_bit_offset;
        } else {
            self.f &= 0b1111_1110 << flag_bit_offset;
        }
    }

    pub fn get_flag(&self, flag: Flag) -> bool {
        self.f & (0b0000_0001 << flag.get_flag_bit_offset()) != 0
    }

    pub fn get_flag_bit(&self, flag: Flag) -> u8 {
        if self.get_flag(flag) {
            1
        } else {
            0
        }
    }

    pub fn set_flags(&mut self, flags: &[Flag], value: bool) {
        for flag in flags.iter() {
            self.set_flag(*flag, value);
        }
    }
}
