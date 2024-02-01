#![allow(dead_code)]

mod cpu;

// TODO: Make instructions for
//  Load registers
// LDA
// LDX
// LDY
//  Inc/dec registers
// DEY
// DEX
// INX
// INY
//  Store registers
// STA
// STX
// STY

// 2 KB ram
const RAM_BYTES: usize = 2 * 1024;
const CARTRIGE_ROM_BYTES: usize = 0xBFE0;

const PRG_ROM_START: usize = 0x4020;

struct Nes {
    cpu: cpu::Cpu,
    memory: AddressSpace,
    is_running: bool,
}

impl Nes {
    fn reset(&mut self) {
        self.cpu.reset(&mut self.memory);
    }
    fn new() -> Self {
        Nes {
            cpu: cpu::Cpu::new(),
            memory: AddressSpace::new(),
            is_running: true,
        }
    }
    fn tick(&mut self) {
        self.cpu.tick(&mut self.memory)
    }
}

struct AddressSpace {
    ram: [u8; RAM_BYTES],
    cartridge: [u8; CARTRIGE_ROM_BYTES],
    // TODO: Mirror stuff from getters
}

impl AddressSpace {
    fn new() -> Self {
        Self {
            ram: [0; RAM_BYTES],
            cartridge: [0; CARTRIGE_ROM_BYTES],
        }
    }

    fn get_byte(&self, address: u16) -> u8 {
        println!("Getting 0x{address:X}");
        // https://www.nesdev.org/wiki/CPU_memory_map
        const RAM_SIZE: u16 = 0x0800;
        const PPU_START: u16 = 0x2000;
        const APU_START: u16 = 0x4000;
        const APU_UNUSED_START: u16 = 0x4018;
        const CARTRIDGE_RAM_START: u16 = 0x4020;
        const CARTRIDGE_ROM_START: u16 = 0x8000;
        const CARTRIDGE_SIZE: u16 = 0xBFE0;
        if address < PPU_START {
            // Mirror addresses to [0x000..0x07FF] range.
            let mirror = (address % RAM_SIZE) as usize;
            self.ram[mirror]
        } else if address < APU_START {
            // PPU
            // Mirror 0x2000 0x2007
            0
            //todo!("PPU not implemented");
        } else if address < APU_UNUSED_START {
            // NES APU
            todo!("APU not implemented");
        } else if address < CARTRIDGE_RAM_START {
            // APU (disabled)
            panic!("APU address space is disabled");
        } else if address < CARTRIDGE_ROM_START {
            todo!()
        } else {
            // Cartridge memory.
            // TODO: Implement mappers
            // Assumed mapper 0
            let address_shifted = address - CARTRIDGE_ROM_START;
            let mirror = address_shifted % (0x8000);
            self.cartridge[mirror as usize]
        }
    }

    fn set_byte(&mut self, address: u16, value: u8) {
        self.ram[address as usize] = value;
    }
}

#[cfg(test)]
mod test {
    use crate::cpu::DEFAULT_PROGRAM_COUNTER;

    use super::*;
    #[test]
    fn test_noop() {
        let mut nes = Nes::new();
        // set ram to no ops;
        let ram = [0xEA; RAM_BYTES];
        nes.memory.ram = ram;
        let num_ticks = 1000;
        for _tick in 0..num_ticks {
            nes.tick();
        }

        // Nop takes 2 clock cycles so program counter should be on start + 1000/2;
        assert_eq!(
            nes.cpu.program_counter(),
            DEFAULT_PROGRAM_COUNTER + 1000 / 2
        );
    }

    #[test]
    fn test_load_registers() {
        let mut nes = Nes::new();
        // Op Codes using immediate addressing. All take 2 cycles
        let lda: u8 = 0xA9;
        let ldx: u8 = 0xA2;
        let ldy: u8 = 0xA0;
        let aaa: u8 = 1;
        let xxx: u8 = 2;
        let yyy: u8 = 3;
        // LDX = 0xA2
        // LDY = 0xA4
        //let program = [ ]
        let program = [
            lda, aaa, // Load 0xF1 into A
            ldx, xxx, // Load 0xF2 into X
            ldy, yyy, // Load 0xF2 into Y
        ];
        println!("program = {program:#?}");
        // Load program into RAM.
        for ii in 0..program.len() {
            nes.memory.ram[DEFAULT_PROGRAM_COUNTER as usize + ii] = program[ii];
        }

        let num_ticks = program.len();

        for _tick in 0..num_ticks {
            nes.tick();
        }
        assert_eq!(nes.cpu.accumulator(), aaa);
        assert_eq!(nes.cpu.register_x(), xxx);
        assert_eq!(nes.cpu.register_y(), yyy);
    }

    #[test]
    fn test_roms() {
        let mut nes = Nes::new();
        let basename = "/home/spencer/dev/rust/nes-test-roms/nes_instr_test/rom_singles/";
        let files = [
            "01-implied.nes",
            "02-immediate.nes",
            "03-zero_page.nes",
            "04-zp_xy.nes",
            "05-absolute.nes",
            "06-abs_xy.nes",
            "07-ind_x.nes",
            "08-ind_y.nes",
            "09-branches.nes",
            "10-stack.nes",
            "11-special.nes",
        ];

        for file in files {
            let file_name = basename.to_string() + file;
            println!("reading rom = {file_name}");
            let nes_file = NesFile::from(&file_name);
            nes_file.print_info();
            let rom = nes_file.prg_rom();

            for (ii, val) in nes.memory.cartridge.iter_mut().enumerate() {
                if ii >= rom.len() {
                    break;
                }
                *val = rom[ii];
            }
            let byte = nes.memory.get_byte(0xFFFE);
            let mut tick = 0;
            nes.reset();
            while nes.is_running {
                if tick > 100000 {
                    break;
                }
                nes.tick();
                tick += 1;
            }
            println!("0x{byte:.X}");
            break;
        }
        panic!();
    }

    // https://www.nesdev.org/wiki/NES_2.0#PRG-ROM_Area
    struct NesFile {
        bytes: Vec<u8>,
        prg_rom: u8,
        prg_rom_size: u8,
        chr_rom: u8,
        chr_rom_size: u8,
    }
    const KILOBYTE: usize = 1024;
    impl NesFile {
        fn prg_rom_size(&self) -> usize {
            if self.prg_rom_size != 0 {
                panic!("PRG ROM SIZE MULTIPLIER NOT HANDLED");
            }
            (self.prg_rom as usize) * 16 * KILOBYTE
        }
        fn chr_rom_size(&self) -> usize {
            if self.chr_rom_size != 0 {
                panic!("CHR ROM SIZE MULTIPLIER NOT HANDLED");
            }
            (self.chr_rom as usize) * 8 * KILOBYTE
        }
        fn prg_rom(&self) -> Vec<u8> {
            let prg_rom_start = 16;
            let prg_rom_end = prg_rom_start + self.prg_rom_size();
            self.bytes[prg_rom_start..prg_rom_end].to_vec()
        }
        fn print_info(&self) {
            let prg_len = self.prg_rom_size();
            let chr_len = self.chr_rom_size();
            let prg_start = 16;
            let prg_end = prg_start + prg_len;
            let chr_start = prg_end + 1;
            let chr_end = chr_start + chr_len;
            println!("header 0x00..0xFF");
            println!("trainer 0xFF..0xFF");
            println!("prg = 0x{prg_start:.X}..0x{prg_end:.X}");
            println!("chr size = 0x{chr_start:.X}..0x{chr_end:.X}");
        }
        fn from<P>(file_name: P) -> Self
        where
            P: AsRef<std::path::Path>,
        {
            let bytes = std::fs::read(file_name).unwrap();
            let is_ines_format =
                bytes[0] == b'N' && bytes[1] == b'E' && bytes[2] == b'S' && bytes[3] == 0x1A;
            let _is_ines_2_format = is_ines_format && bytes[7] == 0x08;

            let prg_rom = bytes[4];
            let chr_rom = bytes[5];

            let prg_rom_size = bytes[9] >> 4;
            let chr_rom_size = bytes[9] & 0x0F;

            assert!(is_ines_format);
            Self {
                bytes,
                prg_rom,
                chr_rom,
                prg_rom_size,
                chr_rom_size,
            }
        }
    }
}

fn main() {
    println!("Hello, world!");
}
