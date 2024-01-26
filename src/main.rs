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

struct Bus {
    cpu: cpu::Cpu,
    ram: [u8; RAM_BYTES],
}

impl Bus {
    fn new() -> Self {
        Bus {
            cpu: cpu::Cpu::new(),
            ram: [0; RAM_BYTES],
        }
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn test_load_registers() {
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
