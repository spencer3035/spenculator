use super::AddressingMode;
use super::Cpu;
use crate::AddressSpace;
pub fn run_addressing(cpu: &mut Cpu, memory: &mut AddressSpace, mode: &AddressingMode) -> bool {
    let zero_page_offset =
        |val: u8| memory.get_byte(cpu.program_counter) as u16 + val as u16 & 0x00FF;

    let absolute_address_offset = |val: u8| {
        let low = memory.get_byte(cpu.program_counter);
        let high = memory.get_byte(cpu.program_counter + 1);
        let mut addr_abs = (high as u16) << 8 | (low as u16);
        addr_abs += val as u16;
        let new_page = if let Some(addr) = cpu.addr_abs {
            addr & 0xFF00 != (high as u16) << 8
        } else {
            false
        };
        (addr_abs, new_page)
    };

    match mode {
        AddressingMode::XXX => {
            cpu.program_counter += 1;
            false
        }
        AddressingMode::IMM => {
            cpu.addr_abs = Some(cpu.program_counter + 1);
            cpu.program_counter += 2;
            false
        }
        AddressingMode::ZP0 => {
            // The cast from u8 to u16 ensures it is on page zero.
            cpu.addr_abs = Some(memory.get_byte(cpu.program_counter) as u16);
            cpu.program_counter += 1;
            false
        }
        AddressingMode::ZPX => {
            let addr_abs = zero_page_offset(cpu.register_x);
            cpu.addr_abs = Some(addr_abs);
            cpu.program_counter += 1;
            false
        }
        AddressingMode::ZPY => {
            cpu.addr_abs = Some(zero_page_offset(cpu.register_y));
            cpu.program_counter += 1;
            false
        }
        AddressingMode::ABS => {
            let (addr_abs, _new_page) = absolute_address_offset(0);
            cpu.program_counter += 2;
            cpu.addr_abs = Some(addr_abs);
            false
        }
        AddressingMode::ABX => {
            let (addr_abs, new_page) = absolute_address_offset(cpu.register_x);
            cpu.program_counter += 2;
            cpu.addr_abs = Some(addr_abs);
            new_page
        }
        AddressingMode::ABY => {
            let (addr_abs, new_page) = absolute_address_offset(cpu.register_y);
            cpu.program_counter += 2;
            cpu.addr_abs = Some(addr_abs);
            new_page
        }
        AddressingMode::IND => {
            let low = memory.get_byte(cpu.program_counter) as u16;
            let high = memory.get_byte(cpu.program_counter + 1) as u16;
            let addr_ptr = high << 8 | low;

            // Simulate hardware bug
            let addr_abs = if low == 0x00FF {
                ((memory.get_byte((addr_ptr & 0xFF00) as u16) as u16) << 8)
                    | (memory.get_byte(addr_ptr) as u16)
            } else {
                (memory.get_byte(addr_ptr + 1) as u16) << 8 | (memory.get_byte(addr_ptr) as u16)
            };

            cpu.addr_abs = Some(addr_abs);
            cpu.program_counter += 2;
            false
        }
        AddressingMode::IZX => {
            let offset_addr = memory.get_byte(cpu.program_counter) as u16 + cpu.register_x as u16;
            let low = memory.get_byte(offset_addr & 0x00FF) as u16;
            let high = memory.get_byte((offset_addr + 1) & 0x00FF) as u16;
            let addr_abs = high << 8 | low;

            cpu.addr_abs = Some(addr_abs);
            cpu.program_counter += 1;
            false
        }
        AddressingMode::IZY => {
            let offset_addr = memory.get_byte(cpu.program_counter) as u16;
            let low = memory.get_byte(offset_addr & 0x00FF) as u16;
            let high = memory.get_byte((offset_addr + 1) & 0x00FF) as u16;
            let addr_abs = (high << 8 | low) + cpu.register_y as u16;

            cpu.addr_abs = Some(addr_abs);
            cpu.program_counter += 1;
            if (addr_abs & 0xFF00) != high << 8 {
                true
            } else {
                false
            }
        }
        AddressingMode::REL => {
            let mut addr_rel = memory.get_byte(cpu.program_counter) as u16;
            if addr_rel & 0x80 != 0 {
                addr_rel |= 0xFF00;
            }
            cpu.addr_rel = Some(addr_rel);
            cpu.program_counter += 1;
            false
        }
    }
}
