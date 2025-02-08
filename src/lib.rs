#![allow(dead_code)]
#![feature(macro_metavar_expr)]

use consts::*;
use utils::split_u16_to_u8s;

mod cpu;
mod nes_file;

pub mod utils {
    use crate::consts::*;

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

    #[inline]
    pub fn is_positive(val: u8) -> bool {
        val & BIT_SEVEN == 0
    }

    #[inline]
    pub fn is_negative(val: u8) -> bool {
        val & BIT_SEVEN != 0
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_positive_negative() {
            assert!(is_positive(0));
            assert!(is_positive(1));
            assert!(is_positive(2));

            assert!(is_negative(0xFF - 0));
            assert!(is_negative(0xFF - 1));
            assert!(is_negative(0xFF - 2));
        }
    }
}
pub mod consts {
    // Binary helper constants
    pub const BIT_SEVEN: u8 = 0b10000000;
    pub const BIT_SIX: u8 = 0b01000000;
    pub const BIT_ZERO: u8 = 0b00000001;

    // Units
    pub const KILOBYTE: usize = 1024;

    // CPU Values
    pub const DEFAULT_PROGRAM_COUNTER: u16 = 0;
    pub const DEFAULT_STACK_POINTER: u8 = 0xFF;
    pub const STACK_BEGIN: u16 = 0x0100;
    pub const STACK_END: u16 = 0x01FF;
    pub const RESET_VECTOR_ADDRESS: u16 = 0xFFFC;
    pub const INTERRUPT_REQUEST_ADDRESS: u16 = 0xFFFE;
    pub const INTERRUPT_NOMASK_ADDRESS: u16 = 0xFFFA;

    // NES Address constants
}

struct Nes {
    cpu: cpu::Cpu,
    memory: Box<dyn AddressSpaceTrait>,
    is_running: bool,
}

impl Nes {
    fn reset(&mut self) {
        self.cpu.reset(self.memory.as_mut());
    }
    fn new() -> Self {
        Nes {
            cpu: cpu::Cpu::new(),
            memory: Box::new(NesAddressSpace::new()),
            is_running: true,
        }
    }
    #[cfg(test)]
    fn new_test() -> Self {
        Nes {
            cpu: cpu::Cpu::new(),
            memory: Box::new(TestAddressSpace::new()),
            is_running: true,
        }
    }
    // Returns false if something fails
    fn tick(&mut self) -> bool {
        self.cpu.tick(self.memory.as_mut())
    }
}

struct Cartridge {
    chr: Vec<u8>,
    prg: Vec<u8>,
    ram: Vec<u8>,
}

impl Cartridge {
    fn new() -> Self {
        Cartridge {
            chr: vec![0; 8 * KILOBYTE],
            prg: vec![0; 32 * KILOBYTE],
            ram: vec![0; CARTRIGE_RAM_BYTES as usize],
        }
    }

    fn write_ram(&mut self, addr: u16, val: u8) {
        let addr_map = (addr % CARTRIGE_RAM_BYTES) as usize;
        self.ram[addr_map] = val;
    }

    fn read_ram(&self, addr: u16) -> u8 {
        let addr_map = (addr as usize) % self.ram.len();
        assert_eq!(addr as usize, addr_map, "TRIED TO READ OUT OF BOUNDS");
        self.ram[addr_map]
    }

    fn read_rom(&self, addr: u16) -> u8 {
        let addr_map = (addr as usize) % self.prg.len();
        assert_eq!(addr as usize, addr_map, "TRIED TO READ OUT OF BOUNDS");
        self.prg[addr_map]
    }

    fn load_chr(&mut self, chr: Vec<u8>) {
        self.chr = chr;
    }
    fn load_prg(&mut self, prg: Vec<u8>) {
        self.prg = prg;
    }
}

trait AddressSpaceTrait: 'static {
    fn get_byte(&mut self, address: u16) -> u8;
    fn set_byte(&mut self, address: u16, value: u8);

    // Provided
    fn set_reset_vector(&mut self, val: u16) {
        let (low, high) = split_u16_to_u8s(val);
        self.set_byte(RESET_VECTOR_ADDRESS, low);
        self.set_byte(RESET_VECTOR_ADDRESS + 1, high);
    }
}

struct TestAddressSpace {
    ram: [u8; 0x10000],
}

impl AddressSpaceTrait for TestAddressSpace {
    fn get_byte(&mut self, address: u16) -> u8 {
        self.ram[address as usize]
    }
    fn set_byte(&mut self, address: u16, value: u8) {
        self.ram[address as usize] = value;
    }
}

impl TestAddressSpace {
    fn new() -> Self {
        TestAddressSpace { ram: [0; 0x10000] }
    }
}

const CARTRIGE_ROM_BYTES: u16 = 0x8000 - 0x6000;
const CARTRIGE_RAM_BYTES: u16 = 0xFFFF - 0x8000 + 1;
const RAM_SIZE: u16 = 0x0800;
const PPU_START: u16 = 0x2000;
const APU_START: u16 = 0x4000;
const APU_UNUSED_START: u16 = 0x4018;
const CARTRIDGE_RAM_START: u16 = 0x4020;
const CARTRIDGE_ROM_START: u16 = 0x8000;
const CARTRIDGE_SIZE: u16 = 0xBFE0;

struct NesAddressSpace {
    // CPU ram
    ram: [u8; RAM_SIZE as usize],
    cartridge: Cartridge,
}

impl NesAddressSpace {
    fn new() -> Self {
        Self {
            ram: [0; RAM_SIZE as usize],
            cartridge: Cartridge::new(),
        }
    }
}

impl AddressSpaceTrait for NesAddressSpace {
    // TODO: Needs to be &mut self eventually. Reading can change state of the Cartridge
    fn get_byte(&mut self, address: u16) -> u8 {
        // https://www.nesdev.org/wiki/CPU_memory_map
        let val = if address < PPU_START {
            // CPU RAM
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
            self.cartridge.read_ram(address - CARTRIDGE_RAM_START)
        } else {
            // Cartridge ROM
            // TODO: Implement mappers
            // Assumed mapper 0
            let address_shifted = address - CARTRIDGE_ROM_START;
            let mirror = address_shifted % 0x8000;
            self.cartridge.read_rom(mirror)
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
            //todo!("PPU not implemented");
        } else if address < APU_UNUSED_START {
            // APU
            //todo!("APU not implemented");
        } else if address < CARTRIDGE_RAM_START {
            // APU (disabled)
            //todo!("APU address space is disabled");
        } else if address < CARTRIDGE_ROM_START {
            // Cartridge RAM
            self.cartridge
                .write_ram(address - CARTRIDGE_RAM_START, value);
        } else {
            // Cartridge memory.
            // TODO: Implement mappers
            // Assumed mapper 0
            //todo!("Cartridge writes not implemented");
            let address_shifted = address - CARTRIDGE_ROM_START;
            let mirror = address_shifted % (0x8000);
            self.cartridge.prg[mirror as usize] = value;
        }
    }
}

#[cfg(test)]
mod test {
    // Test rom repos
    // https://github.com/Klaus2m5/6502_65C02_functional_tests
    // https://github.com/christopherpow/nes-test-roms
    //
    // Helper websites
    //
    // List of all instructions:
    // http://www.6502.org/users/obelisk/6502/reference.html#PHP

    use crate::nes_file::NesFile;

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

    // Halt
    const KIL: u8 = 0x02;

    // Should work
    #[ignore]
    #[test]
    fn test_6502() {
        let file = "./rom_tests/6502_65C02_functional_tests/bin_files/6502_functional_test.bin";
        let mut nes = Nes::new_test();
        for (ii, byte) in std::fs::read(file).unwrap().into_iter().enumerate() {
            nes.memory.set_byte(ii as u16, byte);
        }

        nes.cpu.set_program_counter(0x400);
        loop {
            if !nes.tick() {
                break;
            }
        }
        let test = nes.memory.get_byte(0x200);
        // Final test is 240 at memory address 0x3469
        assert!(test == 240);
        assert_eq!(nes.cpu.program_counter(), 0x3469);
    }

    fn init_nes_with_program(program: &[u8]) -> Nes {
        let mut program = program.to_vec();
        // Halt program after done
        program.push(KIL);
        let mut nes = Nes::new();

        // Set reset vector to start of ROM
        let start = CARTRIDGE_ROM_START;
        nes.memory.set_reset_vector(start);

        // Init NES
        nes.reset();
        assert_eq!(nes.cpu.program_counter(), start);

        // Load program into ROM
        for (ii, val) in program.iter().enumerate() {
            nes.memory.set_byte(start + (ii as u16), *val);
        }

        // Check it is there
        for (ii, val) in program.iter().enumerate() {
            assert_eq!(nes.memory.get_byte(start + (ii as u16)), *val);
        }

        // Return NES
        nes
    }

    #[test]
    fn test_basic_dex() {
        //  LDX #$08
        //decrement:
        //  DEX
        //  STX $0200
        //  CPX #$03
        //  BNE decrement
        //  STX $0201
        //  BRK
        //
        let program = vec![
            0xA2, 0x08, // LDX #$08
            0xCA, // DEX
            0x8E, 0x00, 0x02, // STX $0200
            0xE0, 0x03, // CPX #$03
            0xD0, 0xF8, // BNE -6
            0x8E, 0x01, 0x02, // STX $0201
            0x00, // BRK
        ];
        let mut nes = init_nes_with_program(&program);
        let max_ticks = 10000;
        let mut ticks = 0;
        while nes.tick() {
            if ticks >= max_ticks {
                panic!("Exceeded max ticks!");
            }
            ticks += 1;
        }

        assert_eq!(nes.cpu.register_x(), 3);
    }

    #[test]
    fn test_cart_ram_write() {
        let mut nes = Nes::new();
        nes.reset();

        for ii in 0..CARTRIGE_RAM_BYTES {
            let set_val = (ii % 0xFF) as u8;
            nes.memory.set_byte(CARTRIDGE_RAM_START + ii, set_val);
        }

        for ii in 0..CARTRIGE_RAM_BYTES {
            let val = nes.memory.get_byte(CARTRIDGE_RAM_START + ii);
            let set_val = (ii % 0xFF) as u8;
            assert_eq!(val, set_val);
        }
    }

    #[test]
    fn test_cpu_ram_write() {
        let mut nes = Nes::new();
        nes.reset();

        for ii in 0..RAM_SIZE {
            let set_val = (ii % 0xFE) as u8;
            nes.memory.set_byte(ii as u16, set_val);
        }

        for ii in 0..RAM_SIZE {
            let val = nes.memory.get_byte(ii as u16);
            let set_val = (ii % 0xFE) as u8;
            assert_eq!(val, set_val);
        }
    }

    #[test]
    fn test_stack_push() {
        let a1 = 1;
        let a2 = 2;
        let program = [
            LDA, a1,  // Load into A
            PHA, // Push A to stack
            LDA, a2,  // Load into A
            PLA, // Pull A from stack
            NOP,
        ];
        let mut nes = init_nes_with_program(&program);

        // Do LDA
        assert!(nes.tick());
        assert!(nes.tick());
        assert_eq![nes.cpu.accumulator(), a1];

        // Do PHA
        assert!(nes.tick());
        assert!(nes.tick());
        assert!(nes.tick());

        // Do LDA
        assert!(nes.tick());
        assert!(nes.tick());
        assert_eq![nes.cpu.accumulator(), a2];

        // Do PLA
        assert!(nes.tick());
        assert!(nes.tick());
        assert!(nes.tick());
        assert!(nes.tick());
        assert_eq![nes.cpu.accumulator(), a1];

        assert!(nes.tick());
        assert!(nes.tick());
        assert!(!nes.tick());
    }

    #[test]
    fn test_noop() {
        // set ram to no ops;
        let program = [NOP; 500];
        let mut nes = init_nes_with_program(&program);
        let initial_program_counter = nes.cpu.program_counter();
        let mut num_ticks = 0;
        while nes.tick() {
            num_ticks += 1;
        }

        // Nop takes 2 clock cycles so program counter should be on start + 1000/2, but the last
        // kill instruction takes an additional tick
        let expected_ticks = num_ticks / 2 + 1;
        assert_eq!(
            nes.cpu.program_counter(),
            initial_program_counter + expected_ticks
        );
    }

    #[test]
    fn test_load_registers() {
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
        let mut nes = init_nes_with_program(&program);
        let expected_ticks = 2 + 2 + 2;
        let mut ticks = 0;

        while nes.tick() {
            ticks += 1;
        }
        assert_eq!(expected_ticks, ticks);
        assert_eq!(nes.cpu.accumulator(), aaa);
        assert_eq!(nes.cpu.register_x(), xxx);
        assert_eq!(nes.cpu.register_y(), yyy);
    }

    // NOT WORKING
    #[ignore]
    #[test]
    fn test_nes_roms() {
        let mut nes = Nes::new();
        let basename = "./rom_tests/nes-test-roms/";
        // pha plp php pla
        // x48 x28 x08 x68
        let files = [
            //"cpu_dummy_reads/cpu_dummy_reads.nes",
            //"instr_test-v5/rom_singles/01-basics.nes",
            //"instr_test-v5/official_only.nes",
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
            let nes_file = NesFile::try_from_file(&file_name);
            nes_file.print_info();
            let rom = nes_file.prg_rom().to_vec();

            // Set rom
            for (ii, val) in rom.into_iter().enumerate() {
                let ii = ii as u16;
                nes.memory.set_byte(CARTRIDGE_ROM_START + ii, val)
            }
            let byte = nes.memory.get_byte(0xFFFE);
            let mut tick = 0;
            let program_start = 0x8200;
            nes.memory.set_reset_vector(program_start);
            nes.reset();
            while nes.tick() {
                if tick > 100000 {
                    break;
                }
                tick += 1;
            }
            println!("0x{byte:X}");
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
}
