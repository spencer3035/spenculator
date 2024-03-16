use super::AddressingMode;
use super::Cpu;
use crate::utils::*;
use crate::AddressSpaceTrait;

pub fn run_addressing(
    cpu: &mut Cpu,
    memory: &mut dyn AddressSpaceTrait,
    mode: &AddressingMode,
) -> bool {
    let zero_page_offset = |val: u8, memory: &mut dyn AddressSpaceTrait| {
        memory.get_byte(cpu.program_counter) as u16 + val as u16 & 0x00FF
    };

    let absolute_address_offset = |val: u8, memory: &mut dyn AddressSpaceTrait| {
        let low = memory.get_byte(cpu.program_counter);
        let high = memory.get_byte(cpu.program_counter + 1);
        let mut addr_abs = concat_u8s_to_u16(low, high);
        addr_abs += val as u16;
        let new_page = if let Some(addr) = cpu.addr_abs {
            addr & 0xFF00 != (high as u16) << 8
        } else {
            false
        };
        (addr_abs, new_page)
    };

    match mode {
        AddressingMode::XXX => false,
        AddressingMode::IMM => {
            cpu.addr_abs = Some(cpu.program_counter);
            cpu.program_counter += 1;
            false
        }
        AddressingMode::ZP0 => {
            // The cast from u8 to u16 ensures it is on page zero.
            cpu.addr_abs = Some(memory.get_byte(cpu.program_counter) as u16);
            cpu.program_counter += 1;
            false
        }
        AddressingMode::ZPX => {
            let addr_abs = zero_page_offset(cpu.register_x, memory);
            cpu.addr_abs = Some(addr_abs);
            cpu.program_counter += 1;
            false
        }
        AddressingMode::ZPY => {
            cpu.addr_abs = Some(zero_page_offset(cpu.register_y, memory));
            cpu.program_counter += 1;
            false
        }
        AddressingMode::ABS => {
            let (addr_abs, _new_page) = absolute_address_offset(0, memory);
            cpu.program_counter += 2;
            cpu.addr_abs = Some(addr_abs);
            false
        }
        AddressingMode::ABX => {
            let (addr_abs, new_page) = absolute_address_offset(cpu.register_x, memory);
            cpu.program_counter += 2;
            cpu.addr_abs = Some(addr_abs);
            new_page
        }
        AddressingMode::ABY => {
            let (addr_abs, new_page) = absolute_address_offset(cpu.register_y, memory);
            cpu.program_counter += 2;
            cpu.addr_abs = Some(addr_abs);
            new_page
        }
        AddressingMode::IND => {
            let low = memory.get_byte(cpu.program_counter);
            let high = memory.get_byte(cpu.program_counter + 1);
            let addr_ptr = concat_u8s_to_u16(low, high);

            // Simulate hardware bug
            let addr_abs = if low == 0x00FF {
                let low = memory.get_byte(addr_ptr);
                let high = memory.get_byte(addr_ptr & 0xFF00);
                concat_u8s_to_u16(low, high)
            } else {
                let low = memory.get_byte(addr_ptr);
                let high = memory.get_byte(addr_ptr + 1);
                concat_u8s_to_u16(low, high)
            };

            cpu.addr_abs = Some(addr_abs);
            cpu.program_counter += 2;
            false
        }
        AddressingMode::IZX => {
            let offset_addr = memory.get_byte(cpu.program_counter) as u16 + cpu.register_x as u16;
            let low = memory.get_byte(offset_addr & 0x00FF);
            let high = memory.get_byte((offset_addr + 1) & 0x00FF);
            let addr_abs = concat_u8s_to_u16(low, high);

            cpu.addr_abs = Some(addr_abs);
            cpu.program_counter += 1;
            false
        }
        AddressingMode::IZY => {
            let offset_addr = memory.get_byte(cpu.program_counter) as u16;
            let low = memory.get_byte(offset_addr & 0x00FF);
            let high = memory.get_byte((offset_addr + 1) & 0x00FF);
            let addr_abs = concat_u8s_to_u16(low, high) + cpu.register_y as u16;

            cpu.addr_abs = Some(addr_abs);
            cpu.program_counter += 1;
            if (addr_abs & 0xFF00) != (high as u16) << 8 {
                true
            } else {
                false
            }
        }
        AddressingMode::REL => {
            let addr_rel = memory.get_byte(cpu.program_counter);
            cpu.addr_rel = Some(addr_rel);
            cpu.program_counter += 1;
            false
        }
    }
}
#[cfg(test)]
mod test {
    use crate::consts::*;

    #[test]
    fn test_addr_rel() {
        assert_eq!(0x80, BIT_SEVEN);
    }
}
