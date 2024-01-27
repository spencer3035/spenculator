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
    ram: [u8; RAM_BYTES],
    is_running: bool,
}

impl Nes {
    fn new() -> Self {
        Nes {
            cpu: cpu::Cpu::new(),
            ram: [0; RAM_BYTES],
            is_running: true,
        }
    }
    fn tick(&mut self) {
        self.cpu.tick()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_load_registers() {
        let mut nes = Nes::new();
        let num_ticks = 1000;
        for _tick in 0..num_ticks {
            nes.tick();
        }

        // TODO:
        // LDA = 0xA1
        // LDX = 0xA2
        // LDY = 0xA4
        //let program = [ ]
    }
}

fn main() {
    println!("Hello, world!");
}
