pub struct Mmu {
    pub boot_mode: bool,
    boot_rom: Box<[u8; 0x4000]>,
    tile_ram: Box<[u8; 0x1800]>,
    background_map: Box<[u8; 0x800]>,
    ram: Box<[u8; 0x2000]>,
    oam: Box<[u8; 0xA0]>,
    high_ram: Box<[u8; 0x7F]>,
    cartridge: Cartridge,
}

pub struct Cartridge {
    content: Vec<u8>, // TODO: Vec?
    
}

impl Mmu {
    pub fn read(&self, addr: u16) -> u8 {
        let addr = addr as usize;

        if addr <= 0x00FF && self.boot_mode {
            self.boot_rom[addr]
        } else if addr <= 0x3FFF {
            // TODO: Interrupt table / cartridge header area - special handling?
            todo!("Cartridge ROM")
        } else if addr <= 0x7FFF {
            todo!("Cartridge ROM")
        } else if addr <= 0x97FF {
            self.tile_ram[addr - 0x8000]
        } else if addr <= 0x9FFF {
            self.background_map[addr - 0x9800]
        } else if addr <= 0xBFFF {
            todo!("Cartridge RAM")
        } else if addr <= 0xDFFF {
            self.ram[addr - 0xC000]
        } else if addr <= 0xFDFF {
            self.ram[addr - 0xE000]
        } else if addr <= 0xFE9F {
            self.oam[addr - 0xFE00]
        } else if addr <= 0xFEFF {
            0
        } else if addr <= 0xFF7F {
            todo!("I/O")
        } else if addr <= 0xFFFE {
            self.high_ram[addr - 0xFF80]
        } else {
            // addr == 0xFFFF
            todo!("Interrupt register")
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        let addr = addr as usize;

        if addr <= 0x00FF && self.boot_mode {
            self.boot_rom[addr] = val;
        } else if addr <= 0x3FFF {
            // TODO: Interrupt table / cartridge header area - special handling?
            todo!("Cartridge ROM");
        } else if addr <= 0x7FFF {
            todo!("Cartridge ROM");
        } else if addr <= 0x97FF {
            self.tile_ram[addr - 0x8000] = val;
        } else if addr <= 0x9FFF {
            self.background_map[addr - 0x9800] = val;
        } else if addr <= 0xBFFF {
            todo!("Cartridge RAM");
        } else if addr <= 0xDFFF {
            self.ram[addr - 0xC000] = val;
        } else if addr <= 0xFDFF {
            self.ram[addr - 0xE000] = val;
        } else if addr <= 0xFE9F {
            self.oam[addr - 0xFE00] = val;
        } else if addr <= 0xFEFF {
            // ignore
        } else if addr <= 0xFF7F {
            todo!("I/O");
        } else if addr <= 0xFFFE {
            self.high_ram[addr - 0xFF80] = val;
        } else {
            // addr == 0xFFFF
            todo!("Interrupt register");
        }
    }
}