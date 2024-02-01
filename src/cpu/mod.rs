use crate::AddressSpace;

use self::opcodes::AddressingMode;

mod addressing;
mod instructions;
mod opcodes;

#[derive(Debug)]
pub struct Cpu {
    // Clock
    clock_tick: u8,

    // Registers
    program_counter: u16,
    stack_pointer: u8,
    accumulator: u8,
    register_x: u8,
    register_y: u8,
    status: CpuStatus,

    // Data retrieved from the bus
    fetched_data: Option<u8>,
    // Absolute address received
    addr_abs: Option<u16>,
    // Relative address received
    addr_rel: Option<u16>,
    // Opcode that is currently being processed
    opcode: Option<u8>,
    // Data that needs to be written to memory
    write_data: Option<u8>,
}

pub const DEFAULT_PROGRAM_COUNTER: u16 = 0;
pub const DEFAULT_STACK_POINTER: u8 = 0;

impl Cpu {
    #[cfg(test)]
    pub fn program_counter(&self) -> u16 {
        self.program_counter
    }
    #[cfg(test)]
    pub fn register_x(&self) -> u8 {
        self.register_x
    }
    #[cfg(test)]
    pub fn register_y(&self) -> u8 {
        self.register_y
    }
    #[cfg(test)]
    pub fn accumulator(&self) -> u8 {
        self.accumulator
    }
    pub fn new() -> Self {
        Self {
            clock_tick: 0,
            program_counter: DEFAULT_PROGRAM_COUNTER,
            stack_pointer: DEFAULT_STACK_POINTER,
            accumulator: 0,
            register_x: 0,
            register_y: 0,
            status: CpuStatus::new(),
            fetched_data: None,
            addr_abs: None,
            addr_rel: None,
            opcode: None,
            write_data: None,
        }
    }

    pub fn tick(&mut self, memory: &mut AddressSpace) {
        if self.clock_tick == 0 {
            self.opcode = Some(memory.get_byte(self.program_counter));
            self.clock_tick = self.process_opcode(memory);
        }
        self.clock_tick -= 1;
    }

    pub fn reset(&mut self) {
        *self = Cpu::new();
    }
    fn fetched_data(&self) -> u8 {
        self.fetched_data.expect("fetched_data wasn't set")
    }
    fn addr_abs(&self) -> u16 {
        self.addr_abs.expect("addr_abs wasn't set")
    }
    fn addr_rel(&self) -> u16 {
        self.addr_rel.expect("addr_rel wasn't set")
    }
    fn opcode(&self) -> u8 {
        self.opcode.expect("opcode wasn't set")
    }
    fn write_data(&self) -> u8 {
        self.write_data.expect("write_data wasn't set")
    }
    fn load(&mut self, fetched_data: u8, addr_abs: u16, addr_rel: u16, opcode: u8) {
        self.fetched_data = Some(fetched_data);
        self.addr_abs = Some(addr_abs);
        self.addr_rel = Some(addr_rel);
        self.opcode = Some(opcode);
    }

    fn process_opcode(&mut self, ram: &mut AddressSpace) -> u8 {
        // TODO Load data
        let op = opcodes::OpCode::get(self.opcode());
        let mut clock_cycles_to_wait = op.cycles();
        if self.process_access_mode(ram, op.mode()) {
            clock_cycles_to_wait += 1;
        }
        println!("Running {op:#?}");
        op.instruction_fn(self);
        if op.instruction().is_branch() {
            clock_cycles_to_wait += 1;
        }
        if let Some(data) = self.write_data {
            ram.set_byte(self.addr_abs(), data);
        }
        clock_cycles_to_wait
    }

    fn process_access_mode(&mut self, memory: &mut AddressSpace, mode: &AddressingMode) -> bool {
        let add_extra_cycle = addressing::run_addressing(self, memory, mode);

        if let Some(addr) = self.addr_abs {
            self.fetched_data = Some(memory.get_byte(addr));
        } else if let Some(addr) = self.addr_rel {
            self.fetched_data = Some(memory.get_byte(addr + self.program_counter));
        }
        add_extra_cycle
    }
}

#[derive(Debug)]
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
            fn [<get_ $flag:lower>](&self) -> bool {
                self.get(&CpuStatusFlag::$flag)
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
