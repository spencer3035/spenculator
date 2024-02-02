#![allow(dead_code)]

mod cpu;
pub mod utils {
    #[inline]
    pub fn concat_u8s_to_u16(low: u8, high: u8) -> u16 {
        ((high as u16) << 8) | (low as u16)
    }
    #[inline]
    pub fn split_u16_to_u8s(val: u16) -> (u8, u8) {
        let high = (val >> 8) as u8;
        let low = (val & 0x00FF) as u8;
        (low, high)
    }
}
pub mod consts {
    pub const BIT_SEVEN: u8 = 0b10000000;
    pub const BIT_SIX: u8 = 0b01000000;
    pub const BIT_ZERO: u8 = 0b00000001;

    // CPU Values
    pub const DEFAULT_PROGRAM_COUNTER: u16 = 0;
    pub const DEFAULT_STACK_POINTER: u16 = 0x00;
    pub const STACK_START: u16 = 0x0100;
    pub const RESET_VECTOR_ADDRESS: u16 = 0xFFFC;
    pub const INTERRUPT_REQUEST_ADDRESS: u16 = 0xFFFE;
    pub const INTERRUPT_NOMASK_ADDRESS: u16 = 0xFFFA;
}
use consts::*;

// 2 KB ram
const RAM_BYTES: usize = 2 * 1024;
const CARTRIGE_ROM_BYTES: usize = 0xBFE0;
const CARTRIGE_RAM_BYTES: usize = 0xFFFF;

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
    cartridge_rom: [u8; CARTRIGE_ROM_BYTES],
    cartridge_ram: [u8; CARTRIGE_RAM_BYTES],
    // TODO: Mirror stuff from getters
}

const RAM_SIZE: u16 = 0x0800;
const PPU_START: u16 = 0x2000;
const APU_START: u16 = 0x4000;
const APU_UNUSED_START: u16 = 0x4018;
const CARTRIDGE_RAM_START: u16 = 0x4020;
const CARTRIDGE_ROM_START: u16 = 0x8000;
const CARTRIDGE_SIZE: u16 = 0xBFE0;

impl AddressSpace {
    fn new() -> Self {
        Self {
            ram: [0; RAM_BYTES],
            cartridge_rom: [0; CARTRIGE_ROM_BYTES],
            cartridge_ram: [0; CARTRIGE_RAM_BYTES],
        }
    }

    fn get_byte(&self, address: u16) -> u8 {
        // https://www.nesdev.org/wiki/CPU_memory_map
        let val = if address < PPU_START {
            // Mirror addresses to [0x000..0x07FF] range.
            let mirror = (address % RAM_SIZE) as usize;
            let val = self.ram[mirror];
            val
        } else if address < APU_START {
            // PPU
            // Mirror 0x2000 0x2007
            0
            //todo!("PPU not implemented");
        } else if address < APU_UNUSED_START {
            // NES APU
            //todo!("APU not implemented");
            0x00
        } else if address < CARTRIDGE_RAM_START {
            // APU (disabled)
            //panic!("APU address space is disabled");
            0x00
        } else if address < CARTRIDGE_ROM_START {
            // Cartridge RAM
            self.cartridge_ram[address as usize]
        } else {
            // Cartridge memory.
            // TODO: Implement mappers
            // Assumed mapper 0
            let address_shifted = address - CARTRIDGE_ROM_START;
            let mirror = address_shifted % (0x8000);
            self.cartridge_rom[mirror as usize]
        };
        val
    }

    fn set_byte(&mut self, address: u16, value: u8) {
        if address < PPU_START {
            // RAM
            // Mirror addresses to [0x000..0x07FF] range.
            let mirror = (address % RAM_SIZE) as usize;
            self.ram[mirror] = value;
        } else if address < APU_START {
            // PPU
            todo!("PPU not implemented");
        } else if address < APU_UNUSED_START {
            // APU
            todo!("APU not implemented");
        } else if address < CARTRIDGE_RAM_START {
            // APU (disabled)
            todo!("APU address space is disabled");
        } else if address < CARTRIDGE_ROM_START {
            // Cartridge RAM
            self.cartridge_ram[address as usize] = value;
        } else {
            // Cartridge memory.
            // TODO: Implement mappers
            // Assumed mapper 0
            //todo!("Cartridge writes not implemented");
            let address_shifted = address - CARTRIDGE_ROM_START;
            let mirror = address_shifted % (0x8000);
            self.cartridge_rom[mirror as usize] = value;
        }
    }
}

#[cfg(test)]
mod test {
    use crate::utils::*;

    use super::*;
    // Op Codes using immediate addressing. All take 2 cycles
    const LDA: u8 = 0xA9;
    const LDX: u8 = 0xA2;
    const LDY: u8 = 0xA0;

    // 4 Cycles
    const PLA: u8 = 0x68;
    // 3 Cycles
    const PHA: u8 = 0x48;

    // 2 cycles
    const NOP: u8 = 0xEA;

    // 3 cycles
    const PHP: u8 = 0x08;

    #[test]
    fn test_stack_push() {
        let a1 = 1;
        let a2 = 2;
        let mut nes = Nes::new();
        let ram = [NOP; RAM_BYTES];
        nes.memory.ram = ram;
        let start = CARTRIDGE_ROM_START;
        let (low, high) = split_u16_to_u8s(start);
        nes.memory.set_byte(RESET_VECTOR_ADDRESS, low);
        nes.memory.set_byte(RESET_VECTOR_ADDRESS + 1, high);
        let program = [
            LDA, a1,  // Load into A
            PHA, // Push A to stack
            LDA, a2,  // Load into A
            PLA, // Pull A from stack
            NOP,
        ];

        for (ii, val) in program.into_iter().enumerate() {
            nes.memory.set_byte(start + (ii as u16), val);
        }

        nes.reset();
        assert_eq!(nes.cpu.program_counter(), start);

        // Do LDA
        nes.tick();
        nes.tick();
        assert_eq![nes.cpu.accumulator(), a1];

        // Do PHA
        nes.tick();
        nes.tick();
        nes.tick();

        // Do LDA
        nes.tick();
        nes.tick();
        assert_eq![nes.cpu.accumulator(), a2];

        // Do PLA
        nes.tick();
        nes.tick();
        nes.tick();
        nes.tick();
        assert_eq![nes.cpu.accumulator(), a1];
    }

    #[test]
    fn test_noop() {
        let mut nes = Nes::new();
        // set ram to no ops;
        let ram = [NOP; RAM_BYTES];
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
        let aaa: u8 = 1;
        let xxx: u8 = 2;
        let yyy: u8 = 3;
        // LDX = 0xA2
        // LDY = 0xA4
        //let program = [ ]
        let program = [
            LDA, aaa, // Load 0xF1 into A
            LDX, xxx, // Load 0xF2 into X
            LDY, yyy, // Load 0xF2 into Y
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

    //#[test]
    fn test_roms() {
        let mut nes = Nes::new();
        let basename = "/home/spencer/dev/rust/nes-test-roms/";
        //let nes_instr_test  = "";
        let files = [
            "cpu_dummy_reads/cpu_dummy_reads.nes",
            //"nes_instr_test/rom_singles/01-implied.nes",
            //"nes_instr_test/rom_singles/02-immediate.nes",
            //"nes_instr_test/rom_singles/03-zero_page.nes",
            //"nes_instr_test/rom_singles/04-zp_xy.nes",
            //"nes_instr_test/rom_singles/05-absolute.nes",
            //"nes_instr_test/rom_singles/06-abs_xy.nes",
            //"nes_instr_test/rom_singles/07-ind_x.nes",
            //"nes_instr_test/rom_singles/08-ind_y.nes",
            //"nes_instr_test/rom_singles/09-branches.nes",
            //"nes_instr_test/rom_singles/10-stack.nes",
            //"nes_instr_test/rom_singles/11-special.nes",
        ];

        for file in files {
            let file_name = basename.to_string() + file;
            println!("reading rom = {file_name}");
            let nes_file = NesFile::from(&file_name);
            nes_file.print_info();
            let rom = nes_file.prg_rom();

            for (ii, val) in nes.memory.cartridge_rom.iter_mut().enumerate() {
                if ii >= rom.len() {
                    break;
                }
                *val = rom[ii];
            }
            let byte = nes.memory.get_byte(0xFFFE);
            let mut tick = 0;
            nes.reset();
            while nes.is_running {
                if tick > 1000 {
                    break;
                }
                nes.tick();
                tick += 1;
            }
            println!("0x{byte:.X}");
            break;
        }
        let mut str = String::new();
        let mut ii = 0;
        loop {
            let char = nes.memory.get_byte(0x6004 + ii);
            if char == 0 {
                break;
            }
            str.push(char as char);
            ii += 1;
        }
        println!("RESULT: {str}");
        panic!()
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
