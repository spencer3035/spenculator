use super::{opcodes::Instruction, Cpu};

macro_rules! match_instruction {
        ($($ins:ident),*) => {
            pub fn process_instruction(cpu: &mut Cpu, instruction: &Instruction) {
                match instruction {
                    $(
                        Instruction::$ins => ::paste::paste!( [<$ins:lower>]) (cpu),
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

//pub fn ldx(cpu: &mut Cpu, val: &u8) {
//    cpu.register_x = *val;
//    // Zero flag
//    if cpu.register_x == 0 {
//        cpu.status.set_z();
//    }
//    if cpu.register_x & 0b10000000 == 0 {
//        cpu.status.set_n();
//    }
//}

pub fn adc(_cpu: &mut Cpu) {
    todo!()
}
pub fn ahx(_cpu: &mut Cpu) {
    todo!()
}
pub fn alr(_cpu: &mut Cpu) {
    todo!()
}
pub fn anc(_cpu: &mut Cpu) {
    todo!()
}
pub fn and(_cpu: &mut Cpu) {
    todo!()
}
pub fn arr(_cpu: &mut Cpu) {
    todo!()
}
pub fn asl(_cpu: &mut Cpu) {
    todo!()
}
pub fn axs(_cpu: &mut Cpu) {
    todo!()
}
pub fn bcc(_cpu: &mut Cpu) {
    todo!()
}
pub fn bcs(_cpu: &mut Cpu) {
    todo!()
}
pub fn beq(_cpu: &mut Cpu) {
    todo!()
}
pub fn bit(_cpu: &mut Cpu) {
    todo!()
}
pub fn bmi(_cpu: &mut Cpu) {
    todo!()
}
pub fn bne(_cpu: &mut Cpu) {
    todo!()
}
pub fn bpl(_cpu: &mut Cpu) {
    todo!()
}
pub fn brk(_cpu: &mut Cpu) {
    todo!()
}
pub fn bvc(_cpu: &mut Cpu) {
    todo!()
}
pub fn bvs(_cpu: &mut Cpu) {
    todo!()
}
pub fn clc(_cpu: &mut Cpu) {
    todo!()
}
pub fn cld(_cpu: &mut Cpu) {
    todo!()
}
pub fn cli(_cpu: &mut Cpu) {
    todo!()
}
pub fn clv(_cpu: &mut Cpu) {
    todo!()
}
pub fn cmp(_cpu: &mut Cpu) {
    todo!()
}
pub fn cpx(_cpu: &mut Cpu) {
    todo!()
}
pub fn cpy(_cpu: &mut Cpu) {
    todo!()
}
pub fn dcp(_cpu: &mut Cpu) {
    todo!()
}
pub fn dec(_cpu: &mut Cpu) {
    todo!()
}
pub fn dex(_cpu: &mut Cpu) {
    todo!()
}
pub fn dey(_cpu: &mut Cpu) {
    todo!()
}
pub fn eor(_cpu: &mut Cpu) {
    todo!()
}
pub fn inc(_cpu: &mut Cpu) {
    todo!()
}
pub fn inx(_cpu: &mut Cpu) {
    todo!()
}
pub fn iny(_cpu: &mut Cpu) {
    todo!()
}
pub fn isc(_cpu: &mut Cpu) {
    todo!()
}
pub fn jmp(_cpu: &mut Cpu) {
    todo!()
}
pub fn jsr(_cpu: &mut Cpu) {
    todo!()
}
pub fn kil(_cpu: &mut Cpu) {
    todo!()
}
pub fn las(_cpu: &mut Cpu) {
    todo!()
}
pub fn lax(_cpu: &mut Cpu) {
    todo!()
}
pub fn lda(_cpu: &mut Cpu) {
    todo!()
}
pub fn ldx(_cpu: &mut Cpu) {
    todo!()
}
pub fn ldy(_cpu: &mut Cpu) {
    todo!()
}
pub fn lsr(_cpu: &mut Cpu) {
    todo!()
}
pub fn nop(_cpu: &mut Cpu) {
    todo!()
}
pub fn ora(_cpu: &mut Cpu) {
    todo!()
}
pub fn pha(_cpu: &mut Cpu) {
    todo!()
}
pub fn php(_cpu: &mut Cpu) {
    todo!()
}
pub fn pla(_cpu: &mut Cpu) {
    todo!()
}
pub fn plp(_cpu: &mut Cpu) {
    todo!()
}
pub fn rla(_cpu: &mut Cpu) {
    todo!()
}
pub fn rol(_cpu: &mut Cpu) {
    todo!()
}
pub fn ror(_cpu: &mut Cpu) {
    todo!()
}
pub fn rra(_cpu: &mut Cpu) {
    todo!()
}
pub fn rti(_cpu: &mut Cpu) {
    todo!()
}
pub fn rts(_cpu: &mut Cpu) {
    todo!()
}
pub fn sax(_cpu: &mut Cpu) {
    todo!()
}
pub fn sbc(_cpu: &mut Cpu) {
    todo!()
}
pub fn sec(_cpu: &mut Cpu) {
    todo!()
}
pub fn sed(_cpu: &mut Cpu) {
    todo!()
}
pub fn sei(_cpu: &mut Cpu) {
    todo!()
}
pub fn shx(_cpu: &mut Cpu) {
    todo!()
}
pub fn shy(_cpu: &mut Cpu) {
    todo!()
}
pub fn slo(_cpu: &mut Cpu) {
    todo!()
}
pub fn sre(_cpu: &mut Cpu) {
    todo!()
}
pub fn sta(_cpu: &mut Cpu) {
    todo!()
}
pub fn stx(_cpu: &mut Cpu) {
    todo!()
}
pub fn sty(_cpu: &mut Cpu) {
    todo!()
}
pub fn tas(_cpu: &mut Cpu) {
    todo!()
}
pub fn tax(_cpu: &mut Cpu) {
    todo!()
}
pub fn tay(_cpu: &mut Cpu) {
    todo!()
}
pub fn tsx(_cpu: &mut Cpu) {
    todo!()
}
pub fn txa(_cpu: &mut Cpu) {
    todo!()
}
pub fn txs(_cpu: &mut Cpu) {
    todo!()
}
pub fn tya(_cpu: &mut Cpu) {
    todo!()
}
pub fn xaa(_cpu: &mut Cpu) {
    todo!()
}
