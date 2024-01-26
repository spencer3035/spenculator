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

    pub fn clock(&mut self) {
        self.clock_tick += 1;
    }

    pub fn process_opcode(&mut self) {
        // TODO Load data
        let op = opcodes::OpCode::get(self.opcode);
        // TODO: Set with opcode
        let _clock_cycles_to_wait = 5;
        instructions::process_instruction(self, op.instruction());
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
