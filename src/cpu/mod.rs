use crate::consts::*;
use crate::utils::*;
use crate::AddressSpaceTrait;

use self::opcodes::Instruction;
use self::opcodes::{AddressingMode, OpCode};

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
    addr_rel: Option<u8>,
    // Opcode that is currently being processed
    opcode: Option<u8>,
}

impl Cpu {
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
        }
    }

    pub fn print_stack(&self, memory: &dyn AddressSpaceTrait) {
        let mut counter = self.stack_pointer as u16;
        let mut str = String::new();
        let mut str_hex = String::new();
        let mut str_bin = String::new();
        while counter < 0xFF {
            let val = memory.get_byte(STACK_BEGIN + counter + 1);
            str.push_str(&format!("{val} "));
            str_hex.push_str(&format!("0x{val:0>2X} "));
            str_bin.push_str(&format!("0b{val:0>8b} "));
            if counter == 0xFF {
                break;
            }
            counter += 1;
        }
        if !str.is_empty() {
            println!("STACK = {str}");
            println!("STACK = {str_hex}");
            println!("STACK = {str_bin}");
        }
    }

    pub fn debug_print(&self, memory: &dyn AddressSpaceTrait, op: &OpCode, program_counter: u16) {
        let value = {
            let mut str = String::new();
            if let Some(addr) = self.addr_abs {
                str.push_str(&format!(
                    "#${addr:0>4X} ({0:0>3} / 0x{0:0>2X} / 0b{0:0>8b})",
                    self.fetched_data()
                ));
            }
            if let Some(addr) = self.addr_rel {
                let rhs = addr;
                let new_value: i16 = if rhs & BIT_SEVEN == 0 {
                    // Positive
                    rhs as i16
                } else {
                    // Negative
                    ((rhs & !BIT_SEVEN) as i16) - (BIT_SEVEN as i16)
                };
                str.push_str(&format!("{new_value}"));
            }
            // Print accumulator
            if *op.instruction() == Instruction::PLA || *op.instruction() == Instruction::PHA {
                str.push_str(&format!(
                    "   A = ({0:0>3} / 0x{0:0>2X} / 0b{0:0>8b})",
                    self.accumulator
                ));
            }
            // Print status
            if *op.instruction() == Instruction::PLP || *op.instruction() == Instruction::PHP {
                str.push_str(&format!("{:?}", self.status.register));
            }
            str
        };

        // Print address and instruction information
        let test_number = memory.get_byte(0x200);
        if test_number >= 41 {
            println!("0x{:0>4X} : {} {}", program_counter, op.name(), value);
            //println!("a = 0b{:b}", self.accumulator);
            //self.print_stack(memory);
            //println!("{:#?}", self.status);
        }
        //println!("Test = {}", memory.get_byte(0x200));
        // Print other stuff
        //println!("x={}, test={}, sp={}"
        //    self.register_x,
        //    memory.get_byte(0x01),
        //    self.stack_pointer
        //);
    }

    pub fn tick(&mut self, memory: &mut dyn AddressSpaceTrait) -> bool {
        if self.clock_tick == 0 {
            let initial_pc = self.program_counter;
            let opcode_num = memory.get_byte(self.program_counter);
            let opcode = OpCode::get(opcode_num);
            let program_counter = self.program_counter;

            self.program_counter += 1;
            self.clock_tick = self.process_opcode(opcode, memory);
            self.debug_print(memory, opcode, program_counter);

            let final_pc = self.program_counter;
            let infinite_loop = initial_pc == final_pc;
            let halt = *opcode.instruction() == Instruction::KIL;
            if infinite_loop || halt {
                if infinite_loop {
                    println!("Hit infinite loop!");
                    println!("Cpu : {:#?}", self);
                    self.print_stack(memory);
                }
                if halt {
                    println!("Halting.");
                }
                return false;
            }
            self.clear_tmp_variables();
        }
        self.clock_tick -= 1;
        true
    }

    pub fn reset(&mut self, memory: &mut dyn AddressSpaceTrait) {
        *self = Cpu::new();
        // Get reset vector information
        let low = memory.get_byte(RESET_VECTOR_ADDRESS);
        let high = memory.get_byte(RESET_VECTOR_ADDRESS + 1);
        let program_counter = concat_u8s_to_u16(low, high);
        self.program_counter = program_counter;
    }
    fn clear_tmp_variables(&mut self) {
        self.fetched_data = None;
        self.addr_abs = None;
        self.addr_rel = None;
        self.opcode = None;
    }
    fn fetched_data(&self) -> u8 {
        self.fetched_data.expect("fetched_data wasn't set")
    }
    fn addr_abs(&self) -> u16 {
        self.addr_abs.expect("addr_abs wasn't set")
    }
    fn addr_rel_add(&mut self) {
        let addr_rel = self.addr_rel.expect("addr_rel wasn't set");
        let rhs = addr_rel;
        self.program_counter = if rhs & BIT_SEVEN == 0 {
            // Positive
            self.program_counter + (rhs as u16)
        } else {
            // Negative
            (self.program_counter + ((rhs & !BIT_SEVEN) as u16)) - (BIT_SEVEN as u16)
        };
    }
    fn opcode(&self) -> u8 {
        self.opcode.expect("opcode wasn't set")
    }

    fn process_opcode(&mut self, op: &OpCode, memory: &mut dyn AddressSpaceTrait) -> u8 {
        // TODO Load data
        let mut clock_cycles_to_wait = op.cycles();
        if self.process_access_mode(memory, op.mode()) {
            clock_cycles_to_wait += 1;
        }
        op.instruction_fn(self, memory);
        if op.instruction().is_branch() {
            clock_cycles_to_wait += 1;
        }
        // May want to remove these later. Good for debugging, but not strictly required.
        clock_cycles_to_wait
    }

    fn process_access_mode(
        &mut self,
        memory: &mut dyn AddressSpaceTrait,
        mode: &AddressingMode,
    ) -> bool {
        let add_extra_cycle = addressing::run_addressing(self, memory, mode);

        if let Some(addr) = self.addr_abs {
            self.fetched_data = Some(memory.get_byte(addr));
        } else if let Some(addr) = self.addr_rel {
            self.fetched_data =
                Some(memory.get_byte(self.program_counter.wrapping_add(addr as u16)));
        }
        add_extra_cycle
    }
}

struct CpuStatus {
    register: u8,
}

use std::fmt;
impl fmt::Debug for CpuStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut str = String::new();

        if self.get_c() {
            str.push('C');
        } else {
            str.push('c');
        }
        if self.get_z() {
            str.push('Z');
        } else {
            str.push('z');
        }
        if self.get_i() {
            str.push('I');
        } else {
            str.push('i');
        }
        if self.get_d() {
            str.push('D');
        } else {
            str.push('d');
        }
        if self.get_b() {
            str.push('B');
        } else {
            str.push('b');
        }
        if self.get_u() {
            str.push('U');
        } else {
            str.push('u');
        }
        if self.get_v() {
            str.push('V');
        } else {
            str.push('v');
        }
        if self.get_n() {
            str.push('N');
        } else {
            str.push('n');
        }

        f.debug_struct("CpuStatus").field("status", &str).finish()
    }
}

// TODO: Make "set" take bool instead of set/unset
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

    impl Cpu {
        pub fn program_counter(&self) -> u16 {
            self.program_counter
        }
        pub fn stack_pointer(&self) -> u8 {
            self.stack_pointer
        }
        pub fn set_program_counter(&mut self, val: u16) {
            self.program_counter = val;
        }
        pub fn register_x(&self) -> u8 {
            self.register_x
        }
        pub fn register_y(&self) -> u8 {
            self.register_y
        }
        pub fn accumulator(&self) -> u8 {
            self.accumulator
        }
    }

    #[test]
    fn test_addr_rel() {
        let mut cpu = Cpu::new();
        for addr in 0x00..0xFF {
            let is_negative = addr >= 128;

            let start: u16 = 300;
            let expected = if is_negative {
                start + (addr & !BIT_SEVEN) as u16 - 128
            } else {
                start + addr as u16
            };

            cpu.addr_rel = Some(addr);
            cpu.program_counter = start;
            cpu.addr_rel_add();
            assert_eq!(cpu.program_counter, expected);
        }
    }

    #[test]
    fn test_concat_and_split() {
        for val in 0..u16::MAX {
            let (low, high) = split_u16_to_u8s(val);
            let new_val = concat_u8s_to_u16(low, high);
            assert_eq!(val, new_val);
        }
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
