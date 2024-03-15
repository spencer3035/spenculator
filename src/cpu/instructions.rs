use super::*;

macro_rules! match_instruction {
        ($($ins:ident),*) => {
            pub fn process_instruction(
                cpu: &mut Cpu,
                instruction: &Instruction,
                io : &mut dyn AddressSpaceTrait) {
                match instruction {
                    $(
                        Instruction::$ins => ::paste::paste!( [<$ins:lower>]) (cpu, io),
                    )*
                }
            }
        };
    }

// Utility functions to make code slightly cleaner
#[inline]
fn cmp_val(cpu: &mut Cpu, val: u8) {
    let c_flag = val >= cpu.fetched_data();
    cpu.status.set_c(c_flag);
    let z_flag = val == cpu.fetched_data();
    cpu.status.set_z(z_flag);
    let n_flag = val.wrapping_sub(cpu.fetched_data()) & BIT_SEVEN != 0;
    cpu.status.set_n(n_flag);
}
#[inline]
fn check_z_and_n(cpu: &mut Cpu, val: u8) {
    check_zero_and_set_z(cpu, val);
    check_negative_and_set_n(cpu, val);
}
#[inline]
fn check_zero_and_set_z(cpu: &mut Cpu, val: u8) {
    let z_flag = val == 0;
    cpu.status.set_z(z_flag);
}
#[inline]
fn check_negative_and_set_n(cpu: &mut Cpu, val: u8) {
    let n_flag = is_negative(val);
    cpu.status.set_n(n_flag);
}
#[inline]
fn write_to_stack(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait, val: u8) {
    let addr = STACK_BEGIN + cpu.stack_pointer as u16;
    if addr < 0x100 {
        panic!("Stack underflow!");
    }
    cpu.stack_pointer = cpu.stack_pointer.wrapping_sub(1);
    io.set_byte(addr, val);
}
#[inline]
fn read_from_stack(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) -> u8 {
    cpu.stack_pointer = cpu.stack_pointer.wrapping_add(1);
    let addr = STACK_BEGIN + cpu.stack_pointer as u16;
    let val = io.get_byte(addr);
    val
}

// Begin actual instruction implementations
pub fn adc(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    if cpu.status.get_d() {
        adc_dec(cpu, _io);
        return;
    }
    let data = cpu.fetched_data();

    let mut c_flag;
    let should_be_positive = is_positive(cpu.accumulator) && is_positive(data);
    let should_be_negative = is_negative(cpu.accumulator) && is_negative(data);
    (cpu.accumulator, c_flag) = cpu.accumulator.overflowing_add(data);
    if cpu.status.get_c() {
        let tmp_carry;
        (cpu.accumulator, tmp_carry) = cpu.accumulator.overflowing_add(1);
        c_flag |= tmp_carry;
    }
    let ended_negative = is_negative(cpu.accumulator);
    let ended_positive = is_positive(cpu.accumulator);

    let v_flag = (should_be_positive && ended_negative) || (should_be_negative && ended_positive);
    cpu.status.set_v(v_flag);
    cpu.status.set_c(c_flag);
    check_z_and_n(cpu, cpu.accumulator);
}
// Decimal mode ADC. Just convert to decimal and manually check overflow
pub fn adc_dec(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    println!("Using decimal!");
    let data = cpu.fetched_data();
    println!("ACC = 0x{:X}", cpu.accumulator);
    println!("DAT = 0x{:X}", data);
    let data = hex_as_decimal(data);
    let acc = hex_as_decimal(cpu.accumulator);
    println!("ACC = {acc}");
    println!("DAT = {data}");

    let mut c_flag;
    let mut res;
    (res, c_flag) = acc.overflowing_add(data);
    println!("itermediate {res}");
    if cpu.status.get_c() {
        res += 1;
    }
    println!("itermediate {res}");
    if res >= 100 {
        c_flag = true;
        res -= 100;
    }
    println!("itermediate {res}");

    println!("RES = {}", res);
    cpu.accumulator = decimal_as_hex(res);
    //cpu.accumulator = res;

    println!("ACC = 0x{:X}", cpu.accumulator);
    cpu.status.set_c(c_flag);
    check_z_and_n(cpu, cpu.accumulator);
}
// Decimal mode SBC. Just convert to decimal and manually check overflow
pub fn sbc_dec(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    println!("DECIMAL SBC");
    let data = cpu.fetched_data();
    let data = hex_as_decimal(data);
    let acc = hex_as_decimal(cpu.accumulator);

    let mut c_flag;
    let mut res;
    println!("ACC = {}", acc);
    println!("DAT = {}", data);
    (res, c_flag) = acc.overflowing_sub(data);
    println!("RES = {}", res);
    if !cpu.status.get_c() {
        println!("Carry!");
        let tmp_flag;
        (res, tmp_flag) = res.overflowing_sub(1);
        c_flag |= tmp_flag;
    }
    println!("RES = {}", res);
    if c_flag {
        // Shift 255 to 99
        res -= 156;
    }

    cpu.accumulator = decimal_as_hex(res);

    println!("ACC = 0x{:X}", cpu.accumulator);
    if c_flag {
        println!("Setting C flag!");
    }
    cpu.status.set_c(!c_flag);
    check_z_and_n(cpu, cpu.accumulator);
}
pub fn ahx(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn alr(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn anc(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn and(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    let data = cpu.fetched_data();
    cpu.accumulator &= data;
    check_z_and_n(cpu, cpu.accumulator);
}
pub fn arr(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn asl(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    // Flags don't work for acc = 0
    let shift_value = |cpu: &mut Cpu, val: u8| -> u8 {
        let c_flag = val & BIT_SEVEN != 0;
        cpu.status.set_c(c_flag);
        let val = val << 1;
        check_z_and_n(cpu, val);
        val
    };

    if let Some(data) = cpu.fetched_data {
        let val = shift_value(cpu, data);
        io.set_byte(cpu.addr_abs(), val);
    } else {
        let val = shift_value(cpu, cpu.accumulator);
        cpu.accumulator = val;
    }
}
pub fn axs(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn bcc(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    if !cpu.status.get_c() {
        // branch
        cpu.addr_rel_add();
    }
}
pub fn bcs(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    if cpu.status.get_c() {
        cpu.addr_rel_add();
    }
}
pub fn beq(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    if cpu.status.get_z() {
        cpu.addr_rel_add();
    }
}
pub fn bit(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    let data = cpu.fetched_data();
    let result = cpu.accumulator & data;
    check_zero_and_set_z(cpu, result);
    let v_flag = data & BIT_SIX != 0;
    cpu.status.set_v(v_flag);
    let n_flag = data & BIT_SEVEN != 0;
    cpu.status.set_n(n_flag);
}
pub fn bmi(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    if cpu.status.get_n() {
        cpu.addr_rel_add();
    }
}
pub fn bne(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    if !cpu.status.get_z() {
        cpu.addr_rel_add();
    }
}
pub fn bpl(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    if !cpu.status.get_n() {
        cpu.addr_rel_add();
    }
}
pub fn brk(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    cpu.status.set_b(true);
    inturrupt_and_set_program_counter(cpu, io, INTERRUPT_REQUEST_ADDRESS);
}
pub fn bvc(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    if !cpu.status.get_v() {
        cpu.addr_rel_add();
    }
}
pub fn bvs(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    if cpu.status.get_v() {
        cpu.addr_rel_add();
    }
}
pub fn clc(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.status.set_c(false);
}
pub fn cld(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.status.set_d(false);
}
pub fn cli(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.status.set_i(false);
}
pub fn clv(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.status.set_v(false);
}
pub fn cmp(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cmp_val(cpu, cpu.accumulator);
}
pub fn cpx(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cmp_val(cpu, cpu.register_x);
}
pub fn cpy(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cmp_val(cpu, cpu.register_y);
}
pub fn dcp(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn dec(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    let result = cpu.fetched_data().wrapping_sub(1);
    check_z_and_n(cpu, result);
    io.set_byte(cpu.addr_abs(), result);
}
pub fn dex(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.register_x = cpu.register_x.wrapping_sub(1);
    check_z_and_n(cpu, cpu.register_x);
}
pub fn dey(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.register_y = cpu.register_y.wrapping_sub(1);
    check_z_and_n(cpu, cpu.register_y);
}
pub fn eor(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.accumulator ^= cpu.fetched_data();
    check_z_and_n(cpu, cpu.accumulator);
}
pub fn inc(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    let result = cpu.fetched_data().wrapping_add(1);
    check_z_and_n(cpu, result);
    io.set_byte(cpu.addr_abs(), result);
}
pub fn inx(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.register_x = cpu.register_x.wrapping_add(1);
    check_z_and_n(cpu, cpu.register_x);
}
pub fn iny(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.register_y = cpu.register_y.wrapping_add(1);
    check_z_and_n(cpu, cpu.register_y);
}
pub fn isc(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn jmp(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.program_counter = cpu.addr_abs();
}
pub fn jsr(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    let (low, high) = split_u16_to_u8s(cpu.program_counter - 1);
    cpu.program_counter = cpu.addr_abs();
    write_to_stack(cpu, io, high);
    write_to_stack(cpu, io, low);
}
pub fn kil(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {}
pub fn las(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn lax(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn lda(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.accumulator = cpu.fetched_data();
    check_z_and_n(cpu, cpu.accumulator);
}
pub fn ldx(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.register_x = cpu.fetched_data();
    check_z_and_n(cpu, cpu.register_x);
}
pub fn ldy(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.register_y = cpu.fetched_data();
    check_z_and_n(cpu, cpu.register_y);
}
pub fn lsr(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    let shift_value = |cpu: &mut Cpu, val: u8| -> u8 {
        let c_flag = val & BIT_ZERO != 0;
        cpu.status.set_c(c_flag);
        let val = val >> 1;
        check_z_and_n(cpu, val);
        val
    };

    if let Some(data) = cpu.fetched_data {
        let val = shift_value(cpu, data);
        io.set_byte(cpu.addr_abs(), val);
    } else {
        let val = shift_value(cpu, cpu.accumulator);
        cpu.accumulator = val;
    }
}
pub fn nop(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {}
pub fn ora(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.accumulator |= cpu.fetched_data();
    check_z_and_n(cpu, cpu.accumulator);
}
pub fn pha(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    write_to_stack(cpu, io, cpu.accumulator);
}
pub fn php(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    // This break command is not documented here, but it is expected:
    // http://www.6502.org/users/obelisk/6502/reference.html#PHP
    cpu.status.set_b(true);
    cpu.status.set_u(true);
    write_to_stack(cpu, io, cpu.status.register);
}
pub fn pla(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    let val = read_from_stack(cpu, io);
    cpu.accumulator = val;
    check_z_and_n(cpu, cpu.accumulator);
}
pub fn plp(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    cpu.status.register = read_from_stack(cpu, io);
    cpu.status.set_u(true);
}
pub fn rla(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn rol(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    let shift_value = |cpu: &mut Cpu, val: u8| -> u8 {
        let is_carry = cpu.status.get_c();
        let c_flag = val & BIT_SEVEN != 0;
        cpu.status.set_c(c_flag);
        let mut val = val << 1;
        if is_carry {
            val = val | BIT_ZERO;
        }
        check_z_and_n(cpu, val);
        val
    };

    if let Some(data) = cpu.fetched_data {
        let val = shift_value(cpu, data);
        io.set_byte(cpu.addr_abs(), val);
    } else {
        let val = shift_value(cpu, cpu.accumulator);
        cpu.accumulator = val;
    }
}
pub fn ror(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    let shift_value = |cpu: &mut Cpu, val: u8| -> u8 {
        let is_carry = cpu.status.get_c();
        let c_flag = val & BIT_ZERO != 0;
        cpu.status.set_c(c_flag);
        let mut val = val >> 1;
        if is_carry {
            val = val | BIT_SEVEN;
        }
        check_z_and_n(cpu, val);
        val
    };

    if let Some(data) = cpu.fetched_data {
        let val = shift_value(cpu, data);
        io.set_byte(cpu.addr_abs(), val);
    } else {
        let val = shift_value(cpu, cpu.accumulator);
        cpu.accumulator = val;
    }
}
pub fn rra(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn rti(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    cpu.status.register = read_from_stack(cpu, io);
    cpu.status.set_u(true);
    let low = read_from_stack(cpu, io);
    let high = read_from_stack(cpu, io);
    cpu.program_counter = concat_u8s_to_u16(low, high);
}
pub fn rts(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    // Get subroutine address from stack and set program counter to it
    let low = read_from_stack(cpu, io);
    let high = read_from_stack(cpu, io);
    cpu.program_counter = concat_u8s_to_u16(low, high) + 1;
}
pub fn sax(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn sbc(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    if cpu.status.get_d() {
        sbc_dec(cpu, _io);
    } else {
        // Trick to use adc. Invert all bits of fetched data to add a negative number
        cpu.fetched_data = Some(cpu.fetched_data() ^ 0xFF);
        adc(cpu, _io);
    }
}
pub fn sec(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.status.set_c(true);
}
pub fn sed(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.status.set_d(true);
}
pub fn sei(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.status.set_i(true);
}
pub fn shx(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn shy(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn slo(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn sre(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn sta(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    io.set_byte(cpu.addr_abs(), cpu.accumulator);
}
pub fn stx(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    io.set_byte(cpu.addr_abs(), cpu.register_x);
}
pub fn sty(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    io.set_byte(cpu.addr_abs(), cpu.register_y);
}
pub fn tas(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn tax(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.register_x = cpu.accumulator;
    check_z_and_n(cpu, cpu.register_x);
}
pub fn tay(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.register_y = cpu.accumulator;
    check_z_and_n(cpu, cpu.register_y);
}
pub fn tsx(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.register_x = cpu.stack_pointer;
    check_z_and_n(cpu, cpu.register_x);
}
pub fn txa(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.accumulator = cpu.register_x;
    check_z_and_n(cpu, cpu.accumulator);
}
pub fn txs(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.stack_pointer = cpu.register_x;
}
pub fn tya(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.accumulator = cpu.register_y;
    check_z_and_n(cpu, cpu.accumulator);
}
pub fn xaa(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}

/// Interrupt request
pub fn irq(cpu: &mut Cpu, memory: &mut dyn AddressSpaceTrait) {
    if cpu.status.get_i() {
        inturrupt_and_set_program_counter(cpu, memory, INTERRUPT_REQUEST_ADDRESS);
    }
}
// Non maskable interrupt
pub fn nmi(cpu: &mut Cpu, memory: &mut dyn AddressSpaceTrait) {
    inturrupt_and_set_program_counter(cpu, memory, INTERRUPT_NOMASK_ADDRESS);
}
fn inturrupt_and_set_program_counter(
    cpu: &mut Cpu,
    io: &mut dyn AddressSpaceTrait,
    pc_address: u16,
) {
    // Write PC and status to stack
    let return_pc = cpu.program_counter + 1;
    let (low, high) = split_u16_to_u8s(return_pc);
    write_to_stack(cpu, io, high);
    write_to_stack(cpu, io, low);
    write_to_stack(cpu, io, cpu.status.register);

    // Set IRQ flags
    cpu.status.set_b(false);
    cpu.status.set_u(true);
    cpu.status.set_i(true);

    // Get new PC address and set.
    let low = io.get_byte(pc_address);
    let high = io.get_byte(pc_address + 1);
    cpu.program_counter = concat_u8s_to_u16(low, high);
}

#[inline]
fn hex_as_decimal(val: u8) -> u8 {
    (val & 0x0F) + 10 * (val >> 4)
}

#[inline]
fn decimal_as_hex(val: u8) -> u8 {
    let ones = val % 10;
    let tens = val / 10;
    (tens << 4) + ones
}

#[cfg(test)]
mod test {
    use crate::TestAddressSpace;

    use super::*;

    #[test]
    fn test_decimal() {
        assert_eq!(5, hex_as_decimal(0x5));
        assert_eq!(10, hex_as_decimal(0x10));
        assert_eq!(15, hex_as_decimal(0x15));
        assert_eq!(25, hex_as_decimal(0x25));
        assert_eq!(99, hex_as_decimal(0x99));

        assert_eq!(0x5, decimal_as_hex(5));
        assert_eq!(0x10, decimal_as_hex(10));
        assert_eq!(0x15, decimal_as_hex(15));
        assert_eq!(0x25, decimal_as_hex(25));
        assert_eq!(0x99, decimal_as_hex(99));
    }

    #[test]
    fn test_instructions() {
        let mut cpu = Cpu::new();
        let mut io = TestAddressSpace::new();

        let check_adc = |cpu: &mut Cpu, io: &mut TestAddressSpace, data, acc, res| {
            cpu.status.set_d(true);
            cpu.fetched_data = Some(data);
            cpu.accumulator = acc;
            adc(cpu, io);
            cpu.fetched_data = None;
            assert_eq!(cpu.accumulator, res);
            cpu.status.set_d(false);
        };

        let check_sbc = |cpu: &mut Cpu, io: &mut TestAddressSpace, data, acc, res| {
            cpu.status.set_d(true);
            cpu.fetched_data = Some(data);
            cpu.accumulator = acc;
            sbc(cpu, io);
            cpu.fetched_data = None;
            assert_eq!(cpu.accumulator, res);
            cpu.status.set_d(false);
        };

        // Note that carry bit is set and unset by calls to ADC
        println!("Checking ADC");
        check_adc(&mut cpu, &mut io, 0x99, 0x01, 0x0);
        assert!(cpu.status.get_c());
        check_adc(&mut cpu, &mut io, 0x99, 0x01, 0x01);
        assert!(cpu.status.get_c());
        check_adc(&mut cpu, &mut io, 0x09, 0x01, 0x11);
        assert!(!cpu.status.get_c());
        check_adc(&mut cpu, &mut io, 0x09, 0x01, 0x10);
        assert!(!cpu.status.get_c());
        check_adc(&mut cpu, &mut io, 0x90, 0x99, 0x89);

        // Note that carry bit is set and unset by calls to SBC
        cpu.status.set_c(false);
        println!("Checking SBC");
        check_sbc(&mut cpu, &mut io, 0x02, 0x01, 0x98);
        assert!(!cpu.status.get_c());
        cpu.status.set_c(true);
        check_sbc(&mut cpu, &mut io, 0x01, 0x10, 0x09);
        assert!(cpu.status.get_c());
        cpu.status.set_c(true);
        check_sbc(&mut cpu, &mut io, 0x99, 0x98, 0x99);
        assert!(!cpu.status.get_c());
        cpu.status.set_c(true);
        check_sbc(&mut cpu, &mut io, 0x0, 0x99, 0x99);
        assert!(cpu.status.get_c());

        println!("Checking 0");
        cpu.fetched_data = Some(1);
        let val = 0;
        cmp_val(&mut cpu, val);
        cpu.fetched_data = None;
        assert!(cpu.status.get_n());

        println!("Checking 1");
        let val = 1;
        check_z_and_n(&mut cpu, val);
        assert!(!cpu.status.get_z());
        assert!(!cpu.status.get_n());

        println!("Checking 0");
        let val = 0;
        check_z_and_n(&mut cpu, val);
        assert!(cpu.status.get_z());
        assert!(!cpu.status.get_n());

        println!("Checking 0xFF");
        let val = 0xFF;
        check_z_and_n(&mut cpu, val);
        assert!(!cpu.status.get_z());
        assert!(cpu.status.get_n());

        println!("Checking BIT_SEVEN");
        let val = 0b10000000;
        check_z_and_n(&mut cpu, val);
        assert!(!cpu.status.get_z());
        assert!(cpu.status.get_n());

        println!("Checking MAX POS");
        let val = 0b01111111;
        check_z_and_n(&mut cpu, val);
        assert!(!cpu.status.get_z());
        assert!(!cpu.status.get_n());
    }
}

match_instruction!(
    ADC, AHX, ALR, ANC, AND, ARR, ASL, AXS, BCC, BCS, BEQ, BIT, BMI, BNE, BPL, BRK, BVC, BVS, CLC,
    CLD, CLI, CLV, CMP, CPX, CPY, DCP, DEC, DEX, DEY, EOR, INC, INX, INY, ISC, JMP, JSR, KIL, LAS,
    LAX, LDA, LDX, LDY, LSR, NOP, ORA, PHA, PHP, PLA, PLP, RLA, ROL, ROR, RRA, RTI, RTS, SAX, SBC,
    SEC, SED, SEI, SHX, SHY, SLO, SRE, STA, STX, STY, TAS, TAX, TAY, TSX, TXA, TXS, TYA, XAA
);
