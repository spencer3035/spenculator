use super::opcodes::Instruction;
use super::*;
use crate::AddressSpaceTrait;

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

match_instruction!(
    ADC, AHX, ALR, ANC, AND, ARR, ASL, AXS, BCC, BCS, BEQ, BIT, BMI, BNE, BPL, BRK, BVC, BVS, CLC,
    CLD, CLI, CLV, CMP, CPX, CPY, DCP, DEC, DEX, DEY, EOR, INC, INX, INY, ISC, JMP, JSR, KIL, LAS,
    LAX, LDA, LDX, LDY, LSR, NOP, ORA, PHA, PHP, PLA, PLP, RLA, ROL, ROR, RRA, RTI, RTS, SAX, SBC,
    SEC, SED, SEI, SHX, SHY, SLO, SRE, STA, STX, STY, TAS, TAX, TAY, TSX, TXA, TXS, TYA, XAA
);

// Utility functions to make code slightly cleaner
#[inline]
fn cmp_val(cpu: &mut Cpu, val: u8) {
    if val >= cpu.fetched_data() {
        cpu.status.set_c();
    }
    if val == cpu.fetched_data() {
        cpu.status.set_z();
    }
    if val.wrapping_sub(cpu.fetched_data()) & BIT_SEVEN != 1 {
        cpu.status.set_n();
    }
}
#[inline]
fn check_z_and_n(cpu: &mut Cpu, val: u8) {
    check_zero_and_set_z(cpu, val);
    check_negative_and_set_n(cpu, val);
}
#[inline]
fn check_zero_and_set_z(cpu: &mut Cpu, val: u8) {
    if val == 0 {
        cpu.status.set_z();
    }
}
#[inline]
fn check_negative_and_set_n(cpu: &mut Cpu, val: u8) {
    if val & BIT_SEVEN == 0 {
        cpu.status.set_n();
    }
}
#[inline]
fn check_overflow_ant_set_c(cpu: &mut Cpu, val: u8) {
    if val & BIT_SEVEN == 0 {
        cpu.status.set_n();
    }
}
#[inline]
fn write_to_stack(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait, val: u8) {
    println!("sp = {}", cpu.stack_pointer);
    println!("val = {}", val);
    let addr = STACK_START
        .checked_sub(cpu.stack_pointer as u16)
        .expect("STACK UNDERFLOW");
    cpu.stack_pointer -= 1;
    io.set_byte(addr, val);
}
#[inline]
fn read_from_stack(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) -> u8 {
    cpu.stack_pointer += 1;
    let addr = STACK_START - cpu.stack_pointer as u16;
    let val = io.get_byte(addr);
    println!("Read {val} from stack");
    val
}

// Begin actual instruction implementations
pub fn adc(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    let data = cpu.fetched_data();
    let val = cpu.accumulator.checked_add(data);
    match val {
        Some(v) => {
            cpu.accumulator = v;
        }
        None => {
            cpu.accumulator += data;
            cpu.status.set_c();
            // Calculate overflow
        }
    }

    check_zero_and_set_z(cpu, cpu.accumulator);
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
pub fn asl(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    let data = cpu.fetched_data();
    check_overflow_ant_set_c(cpu, cpu.accumulator);
    cpu.accumulator &= data;
    check_z_and_n(cpu, cpu.accumulator);
}
pub fn axs(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn bcc(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    if !cpu.status.get_c() {
        // branch
        cpu.program_counter += cpu.addr_rel();
    }
}
pub fn bcs(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    if cpu.status.get_c() {
        cpu.program_counter += cpu.addr_rel();
    }
}
pub fn beq(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    if cpu.status.get_z() {
        cpu.program_counter += cpu.addr_rel();
    }
}
pub fn bit(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    let data = cpu.fetched_data();
    cpu.accumulator = cpu.accumulator & data;
    check_zero_and_set_z(cpu, cpu.accumulator);
    if data & BIT_SIX == 0 {
        cpu.status.unset_v();
    } else {
        cpu.status.set_v();
    }
    if data & BIT_SEVEN == 0 {
        cpu.status.unset_n();
    } else {
        cpu.status.set_n();
    }
}
pub fn bmi(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    if cpu.status.get_n() {
        cpu.program_counter += cpu.addr_rel();
    }
}
pub fn bne(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    if !cpu.status.get_z() {
        cpu.program_counter += cpu.addr_rel();
    }
}
pub fn bpl(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    if !cpu.status.get_z() {
        cpu.program_counter += cpu.addr_rel();
    }
}
pub fn brk(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    inturrupt_and_set_program_counter(cpu, io, INTERRUPT_REQUEST_ADDRESS);
}
pub fn bvc(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    if !cpu.status.get_v() {
        cpu.program_counter += cpu.addr_rel();
    }
}
pub fn bvs(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    if cpu.status.get_v() {
        cpu.program_counter += cpu.addr_rel();
    }
}
pub fn clc(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.status.unset_c();
}
pub fn cld(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.status.unset_d();
}
pub fn cli(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.status.unset_i();
}
pub fn clv(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.status.unset_v();
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
    let result = cpu.fetched_data() - 1;
    check_z_and_n(cpu, result);
    io.set_byte(cpu.addr_abs(), result);
}
pub fn dex(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.register_x -= 1;
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
    cpu.register_x += 1;
    check_z_and_n(cpu, cpu.register_x);
}
pub fn iny(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.register_y += 1;
    check_z_and_n(cpu, cpu.register_y);
}
pub fn isc(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn jmp(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.program_counter = cpu.addr_abs();
}
pub fn jsr(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    let (low, high) = split_u16_to_u8s(cpu.program_counter);
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
        if val & BIT_ZERO != 0 {
            cpu.status.set_c();
        }
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
    write_to_stack(cpu, io, cpu.status.register);
}
pub fn pla(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    let val = read_from_stack(cpu, io);
    cpu.accumulator = val;
    check_z_and_n(cpu, cpu.accumulator);
}
pub fn plp(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn rla(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn rol(cpu: &mut Cpu, io: &mut dyn AddressSpaceTrait) {
    let shift_value = |cpu: &mut Cpu, val: u8| -> u8 {
        let is_carry = if val & BIT_SEVEN != 0 {
            cpu.status.set_c();
            true
        } else {
            false
        };
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
        let is_carry = if val & BIT_ZERO != 0 {
            cpu.status.set_c();
            true
        } else {
            false
        };
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
    let low = read_from_stack(cpu, io);
    let high = read_from_stack(cpu, io);
    cpu.program_counter = concat_u8s_to_u16(low, high);
}
pub fn rts(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn sax(_cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    todo!()
}
pub fn sbc(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    let data = cpu.fetched_data();
    let started_negative = cpu.accumulator & BIT_SEVEN != 0;
    if cpu.status.get_c() {
        if let Some(val) = cpu.accumulator.checked_sub(data) {
            cpu.accumulator = val;
        } else {
            cpu.accumulator -= data;
            // Underflow
            cpu.status.unset_c();
        }
    } else {
        if let Some(val) = cpu.accumulator.checked_sub(data + 1) {
            cpu.accumulator = val;
        } else {
            cpu.accumulator -= data + 1;
            // Underflow
            cpu.status.unset_c();
        }
    }
    let ended_positive = cpu.accumulator & BIT_SEVEN == 0;
    if started_negative && ended_positive {
        cpu.status.set_v();
    }
    check_z_and_n(cpu, cpu.accumulator);
}
pub fn sec(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.status.set_c();
}
pub fn sed(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.status.set_d();
}
pub fn sei(cpu: &mut Cpu, _io: &mut dyn AddressSpaceTrait) {
    cpu.status.set_i();
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
    cpu.register_x = cpu.stack_pointer as u8;
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
    let high = (cpu.program_counter >> 8) as u8;
    let low = (cpu.program_counter & 0x00FF) as u8;
    write_to_stack(cpu, io, high);
    write_to_stack(cpu, io, low);
    cpu.status.unset_b();
    cpu.status.set_u();
    cpu.status.set_i();
    write_to_stack(cpu, io, cpu.status.register);
    let low = io.get_byte(pc_address);
    let high = io.get_byte(pc_address + 1);
    cpu.program_counter = concat_u8s_to_u16(low, high);
}
