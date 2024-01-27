use self::opcodes::AddressingMode;

mod instructions;
mod opcodes;

pub struct Cpu {
    // Clock
    clock_tick: u32,

    // Registers
    program_counter: u16,
    stack_pointer: u8,
    accumulator: u8,
    register_x: u8,
    register_y: u8,
    status: CpuStatus,

    // Data retrieved from the bus
    fetched_data: u8,
    // Absolute address received
    addr_abs: u16,
    // Relative address received
    addr_rel: u16,
    // Opcode that is currently being processed
    opcode: u8,
}

impl Cpu {
    pub fn load(&mut self, fetched_data: u8, addr_abs: u16, addr_rel: u16, opcode: u8) {
        self.fetched_data = fetched_data;
        self.addr_abs = addr_abs;
        self.addr_rel = addr_rel;
        self.opcode = opcode;
    }

    pub fn new() -> Self {
        Self {
            clock_tick: 0,
            program_counter: 0,
            stack_pointer: 0,
            accumulator: 0,
            register_x: 0,
            register_y: 0,
            status: CpuStatus::new(),
            fetched_data: 0,
            addr_abs: 0,
            addr_rel: 0,
            opcode: 0,
        }
    }

    pub fn tick(&mut self) {
        self.clock_tick += 1;
    }

    pub fn process_opcode(&mut self, ram: &mut [u8]) -> u8 {
        // TODO Load data
        let op = opcodes::OpCode::get(self.opcode);
        let mut clock_cycles_to_wait = op.cycles();
        if self.process_access_mode(ram, op.mode()) {
            clock_cycles_to_wait += 1;
        }
        instructions::process_instruction(self, op.instruction());
        clock_cycles_to_wait
    }

    fn process_access_mode(&mut self, ram: &mut [u8], mode: &AddressingMode) -> bool {
        let zero_page_offset =
            |val: u8| ram[self.program_counter as usize] as u16 + val as u16 & 0x00FF;

        let absolute_address_offset = |val: u8| {
            let low = ram[self.program_counter as usize];
            let high = ram[self.program_counter as usize + 1];
            let mut addr_abs = (high as u16) << 8 | (low as u16);
            addr_abs += val as u16;
            let new_page = self.addr_abs & 0xFF00 != (high as u16) << 8;
            (addr_abs, new_page)
        };

        match mode {
            AddressingMode::XXX => false,
            AddressingMode::IMM => {
                self.addr_abs = self.program_counter + 1;
                self.program_counter += 1;
                false
            }
            AddressingMode::ZP0 => {
                // The cast from u8 to u16 ensures it is on page zero.
                self.addr_abs = ram[self.program_counter as usize] as u16;
                self.program_counter += 1;
                false
            }
            AddressingMode::ZPX => {
                let addr_abs = zero_page_offset(self.register_x);
                self.addr_abs = addr_abs;
                self.program_counter += 1;
                false
            }
            AddressingMode::ZPY => {
                self.addr_abs = zero_page_offset(self.register_y);
                self.program_counter += 1;
                false
            }
            AddressingMode::ABS => {
                let (addr_abs, _new_page) = absolute_address_offset(0);
                self.program_counter += 2;
                self.addr_abs = addr_abs;
                false
            }
            AddressingMode::ABX => {
                let (addr_abs, new_page) = absolute_address_offset(self.register_x);
                self.program_counter += 2;
                self.addr_abs = addr_abs;
                new_page
            }
            AddressingMode::ABY => {
                let (addr_abs, new_page) = absolute_address_offset(self.register_y);
                self.program_counter += 2;
                self.addr_abs = addr_abs;
                new_page
            }
            AddressingMode::IND => {
                let low = ram[self.program_counter as usize] as u16;
                let high = ram[self.program_counter as usize + 1] as u16;
                let addr_ptr = (high << 8 | low) as usize;

                // Simulate hardware bug
                let addr_abs = if low == 0x00FF {
                    (ram[addr_ptr & 0xFF00] as u16) << 8 | ram[addr_ptr] as u16
                } else {
                    (ram[addr_ptr + 1] as u16) << 8 | ram[addr_ptr] as u16
                };

                self.addr_abs = addr_abs;
                self.program_counter += 2;
                false
            }
            AddressingMode::IZX => {
                let offset_addr =
                    (ram[self.program_counter as usize] as u16 + self.register_x as u16) as usize;
                let low = ram[offset_addr & 0x00FF] as u16;
                let high = ram[(offset_addr + 1) & 0x00FF] as u16;
                let addr_abs = high << 8 | low;

                self.addr_abs = addr_abs;
                self.program_counter += 1;
                false
            }
            AddressingMode::IZY => {
                let offset_addr = (ram[self.program_counter as usize] as u16) as usize;
                let low = ram[offset_addr & 0x00FF] as u16;
                let high = ram[(offset_addr + 1) & 0x00FF] as u16;
                let addr_abs = (high << 8 | low) + self.register_y as u16;

                self.addr_abs = addr_abs;
                self.program_counter += 1;
                if (addr_abs & 0xFF00) != high << 8 {
                    true
                } else {
                    false
                }
            }
            AddressingMode::REL => {
                self.addr_rel = ram[self.program_counter as usize] as u16;
                self.program_counter += 1;
                if self.addr_rel & 0x80 != 0 {
                    self.addr_rel |= 0xFF00;
                }
                false
            }
        }
    }
}

struct CpuStatus {
    register: u8,
}

macro_rules! set_unset_get_def {
    ($($flag:ident),*) => {
        ::paste::paste! {
            $(
            #[doc=concat!("Sets the ", stringify!($flag), " flag in the CPU status bitmask")]
            fn [<set_ $flag:lower>](&mut self) {
                self.set(&CpuStatusFlag::$flag);
            }

            #[doc=concat!("Unsets the ", stringify!($flag), " flag in the CPU status bitmask")]
            fn [<unset_ $flag:lower>](&mut self) {
                self.unset(&CpuStatusFlag::$flag);
            }

            #[doc=concat!("Gets the ", stringify!($flag), " flag in the CPU status bitmask")]
            fn [<get_ $flag:lower>](&self) {
                self.get(&CpuStatusFlag::$flag);
            }
            )*
        }

    };
}

impl CpuStatus {
    set_unset_get_def!(C, Z, I, D, B, U, V, N);
    fn new() -> Self {
        CpuStatus {
            register: 0b00100000,
        }
    }
    fn set(&mut self, flag: &CpuStatusFlag) {
        self.register |= flag.bit();
    }

    fn unset(&mut self, flag: &CpuStatusFlag) {
        self.register &= !flag.bit();
    }

    fn get(&self, flag: &CpuStatusFlag) -> bool {
        self.register & flag.bit() != 0
    }
}

// https://www.nesdev.org/wiki/Status_flags
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
enum CpuStatusFlag {
    C, // Carry
    Z, // Zero
    I, // Interrupt Disable
    D, // Decimal
    B, // Break
    U, // Unused/Unit (always set to 1)
    V, // Overflow
    N, // Negative
}

impl CpuStatusFlag {
    fn bit(&self) -> u8 {
        match self {
            CpuStatusFlag::C => 1,
            CpuStatusFlag::Z => 1 << 1,
            CpuStatusFlag::I => 1 << 2,
            CpuStatusFlag::D => 1 << 3,
            CpuStatusFlag::B => 1 << 4,
            CpuStatusFlag::U => 1 << 5,
            CpuStatusFlag::V => 1 << 6,
            CpuStatusFlag::N => 1 << 7,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn all_flags() -> Vec<CpuStatusFlag> {
        vec![
            CpuStatusFlag::C,
            CpuStatusFlag::Z,
            CpuStatusFlag::I,
            CpuStatusFlag::D,
            CpuStatusFlag::B,
            CpuStatusFlag::U,
            CpuStatusFlag::V,
            CpuStatusFlag::N,
        ]
    }

    #[test]
    fn test_cpu_status_basic() {
        let mut cpu_status = CpuStatus::new();
        let all_flags = all_flags();

        // The O(ne) flag is always set to 1
        assert_eq!(cpu_status.get(&CpuStatusFlag::U), true);

        // Set one flag to zero for consistency
        cpu_status.unset(&CpuStatusFlag::U);

        // Assert everything starts out as not set.
        for flag in all_flags.iter() {
            assert_eq!(cpu_status.get(flag), false);
        }

        // Set all flags one at a time and check that they got set and nothing else.
        for flag in all_flags.iter() {
            cpu_status.set(flag);
            assert_eq!(cpu_status.get(flag), true);
        }

        // Unset all flags on at a time and check they get unset
        for flag in all_flags.iter() {
            cpu_status.unset(flag);
            assert_eq!(cpu_status.get(flag), false);
        }

        // Finally check all flags are not set
        for flag in all_flags.iter() {
            assert_eq!(cpu_status.get(flag), false);
        }

        // Set one flag at a time and assert all other ones are unset
        for flag_set in all_flags.iter() {
            cpu_status.set(flag_set);

            for flag_unset in all_flags.iter() {
                if flag_unset != flag_set {
                    assert_eq!(cpu_status.get(flag_unset), false);
                } else {
                    assert_eq!(cpu_status.get(flag_set), true);
                }
            }
            cpu_status.unset(flag_set);
            assert_eq!(cpu_status.get(flag_set), false);
        }
    }
}
