pub struct Registers {
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub h: u8,
    pub l: u8,
    f: u8,
}

impl Registers {
    pub fn read_af(&self) -> u16 {
        (self.a as u16) << 8 | (self.f as u16)
    }

    pub fn read_bc(&self) -> u16 {
        (self.b as u16) << 8 | (self.c as u16)
    }

    pub fn read_de(&self) -> u16 {
        (self.d as u16) << 8 | (self.e as u16)
    }

    pub fn read_hl(&self) -> u16 {
        (self.h as u16) << 8 | (self.l as u16)
    }

    pub fn read_f(&self) -> u8 {
        self.f & 0xF0
    }

    pub fn write_af(&mut self, value: u16) {
        self.a = (value >> 8) as u8;
        self.f = value as u8;
    }

    pub fn write_bc(&mut self, value: u16) {
        self.a = (value >> 8) as u8;
        self.f = value as u8;
    }

    pub fn write_de(&mut self, value: u16) {
        self.a = (value >> 8) as u8;
        self.f = value as u8;
    }

    pub fn write_hl(&mut self, value: u16) {
        self.a = (value >> 8) as u8;
        self.f = value as u8;
    }

    pub fn write_f(&mut self, value: u8) {
        self.f = value & 0xF0;
    }

    pub fn set_zero_flag(&mut self, value: bool) {
        if value {
            self.f |= 0b1000_0000;
        } else {
            self.f &= 0b0111_1111;
        }
    }

    pub fn set_subtraction_flag(&mut self, value: bool) {
        if value {
            self.f |= 0b0100_0000;
        } else {
            self.f &= 0b1011_1111;
        }
    }

    pub fn set_halfcarry_flag(&mut self, value: bool) {
        if value {
            self.f |= 0b0010_0000;
        } else {
            self.f &= 0b1101_1111;
        }
    }

    pub fn set_carry_flag(&mut self, value: bool) {
        if value {
            self.f |= 0b0001_0000;
        } else {
            self.f &= 0b1110_1111;
        }
    }
}
