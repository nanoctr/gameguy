use log::warn;

/*
Memory map (source: Gameboy Development Manual, all numbers in hex format):
* 0000 - 00FF: Boot ROM, when in boot mode, otherwise:
* 0000 - 7FFF: Cartridge ROM
  * 0000 - 00FF: RST instructions destination address, starting address for interrupts
  * 0100 - 014F: Metadata about Cartridge, such as game name
  * 0150 - 7FFF: Game data
* 8000 - 9FFF: Display RAM
  * 8000 - 97FF: Character (tile) data; tiles are graphical background patterns that can be drawn and are defined in this area.
    Essentially sprites, although on the Gameboy platform, the term 'Sprite' is used specifically for another type of tile that's not used for background drawing.
  * 9800 - 9FFF: Display data; this area is used to define where the sprites from the tile data memory should be displayed on the screen
* A000 - BFFF: External extension RAM; this is RAM included on the cartridge
* C000 - DFFF: Internal RAM; just good old working memory :)
* E000 - FDFF: Usage of this area is prohibited; shouldn't actually be in use by ROMs. For now, we'll just always read 0 and ignore writes and log when this happens
* FE00 - FFFF: CPU internal RAM
  * FE00 - FE9F: OAM (Object Attribute Memory); Sprites are defined here. These are similar to the tiles from the Display RAM, but instead used to draw characters or interactable objects
  * FF00 - FF7F: TODO
  * FF80 - FFFE: RAM; more (internal) memory, can be used as normal working memory or for the stack
  * FFFF: TODO
*/
pub struct Mmu {
    pub boot_mode: bool,
    boot_rom: [u8; 0x4000],
    // TODO: Maybe move tile_ram and background_map into the PPU later on?
    tile_ram: [u8; 0x1800],
    background_map: [u8; 0x800],
    ram: [u8; 0x2000],
    oam: [u8; 0xA0],
    high_ram: [u8; 0x7F],
}

impl Mmu {
    pub fn read(&self, addr: u16) -> u8 {
        let addr = addr as usize;

        if addr <= 0x00FF && self.boot_mode {
            self.boot_rom[addr]
        } else if addr <= 0x7FFF {
            todo!("Cartridge ROM Bank 0; maybe special handling for cartridge metadata and interrupt addresses")
        } else if addr <= 0x97FF {
            self.tile_ram[addr - 0x8000]
        } else if addr <= 0x9FFF {
            self.background_map[addr - 0x9800]
        } else if addr <= 0xBFFF {
            todo!("Cartridge RAM")
        } else if addr <= 0xDFFF {
            self.ram[addr - 0xC000]
        } else if addr <= 0xFDFF {
            // Usage is prohibited according to developer manual
            warn!("Game tried to read from prohibited memory area at: {addr:X}");
            0
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
        } else if addr <= 0x7FFF {
            todo!("Cartridge ROM Bank 0; maybe special handling for cartridge metadata and interrupt addresses");
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

    pub fn read_u16(&self, addr: u16) -> u16 {
        ((self.read(addr) as u16) << 8) | (self.read(addr + 1) as u16)
    }

    pub fn write_u16(&mut self, addr: u16, val: u16) {
        self.write(addr, (val >> 8) as u8);
        self.write(addr + 1, val as u8);
    }
}
