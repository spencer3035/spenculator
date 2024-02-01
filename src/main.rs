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

struct Nes {
    cpu: cpu::Cpu,
    memory: AddressSpace,
    is_running: bool,
}

struct AddressSpace {
    ram: [u8; RAM_BYTES],
    // TODO: Mirror stuff from getters
}

impl AddressSpace {
    fn new() -> Self {
        Self {
            ram: [0; RAM_BYTES],
        }
    }

    fn get_byte(&self, address: u16) -> u8 {
        self.ram[address as usize]
    }

    fn set_byte(&mut self, address: u16, value: u8) {
        self.ram[address as usize] = value;
    }
}

impl Nes {
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
}

fn main() {
    println!("Hello, world!");
}
