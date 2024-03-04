/// Definition of what an opcode is
use super::Cpu;
use crate::{cpu::instructions::*, AddressSpaceTrait};

pub struct OpCode {
    name: &'static str,
    instruction: Instruction,
    machine_code: u8,
    // TODO: Make a pointer to the function corresponding to the instruction. It may be preferable
    // to get rid of the enum as well because it is a bit cumbersome to use in practice. Attaching
    // everything to the opcode is an attractive idea.
    instruction_fn: &'static dyn Fn(&mut Cpu, &mut dyn AddressSpaceTrait),
    mode: AddressingMode,
    cycles: u8,
    add_cycle_for_new_page: bool,
}

impl std::fmt::Debug for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("OpCode")
            .field("name", &self.name)
            .field("instruction", &self.instruction)
            .field("mode", &self.mode)
            .field("cycles", &self.cycles)
            .field("add_cycle_for_new_page", &self.add_cycle_for_new_page)
            .finish()
    }
}

impl OpCode {
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn instruction(&self) -> &Instruction {
        &self.instruction
    }
    pub fn instruction_fn(&self, cpu: &mut Cpu, address_space: &mut dyn AddressSpaceTrait) {
        (self.instruction_fn)(cpu, address_space);
    }
    pub fn mode(&self) -> &AddressingMode {
        &self.mode
    }
    pub fn cycles(&self) -> u8 {
        self.cycles
    }
    pub fn extra_cycle_for_new_page(&self) -> bool {
        self.add_cycle_for_new_page
    }
    pub const fn get(byte: u8) -> &'static Self {
        &OPCODE_LUT[byte as usize]
    }
}

/// All possible instructions on a 6502 including the undefined opcodes
#[derive(PartialEq, Debug)]
#[rustfmt::skip]
pub enum Instruction {
    ADC, AHX, ALR, ANC, AND, ARR, ASL, AXS, BCC, BCS, BEQ, BIT, BMI, BNE, BPL,
    BRK, BVC, BVS, CLC, CLD, CLI, CLV, CMP, CPX, CPY, DCP, DEC, DEX, DEY, EOR,
    INC, INX, INY, ISC, JMP, JSR, KIL, LAS, LAX, LDA, LDX, LDY, LSR, NOP, ORA,
    PHA, PHP, PLA, PLP, RLA, ROL, ROR, RRA, RTI, RTS, SAX, SBC, SEC, SED, SEI,
    SHX, SHY, SLO, SRE, STA, STX, STY, TAS, TAX, TAY, TSX, TXA, TXS, TYA, XAA,
}

impl Instruction {
    pub fn is_branch(&self) -> bool {
        type I = Instruction;
        match *self {
            I::BPL | I::BMI | I::BVC | I::BVS | I::BCC | I::BCS | I::BNE | I::BEQ => true,
            _ => false,
        }
    }
}

// http://www.emulator101.com/6502-addressing-modes.html
/// All possible addressing modes
#[derive(PartialEq, Debug)]
pub enum AddressingMode {
    ABS, // Absolute addressing mode
    ABX, // Absolute addressing with X offset
    ABY, // Absolute addressing with Y offset
    IMM, // Immediate (next address)
    IND, // Indirect Addressing. Like absolute addressing, but with pointers.
    IZX, // Zero page indexing with X offset
    IZY, // Relative indexing on Y
    REL, // Relative addressing to PC
    XXX, // No addressing defined
    ZP0, // Zero page indexing
    ZPX, // Zero page indexing on X
    ZPY, // Zero page indexing on y
}

// This uses the unstable feature macro_metavar_expr with the ${index()} pattern
macro_rules! format_lut {
    (
        $( ($ins:ident, $addr:ident, $cycles:expr, $extra:expr) ),*
    ) => {{
        [
        $(
            OpCode {
                name : stringify!($ins),
                instruction: Instruction::$ins,
                machine_code: ${index()},
                mode : AddressingMode::$addr,
                instruction_fn : &::paste::paste!([<$ins:lower>]),
                cycles : $cycles,
                add_cycle_for_new_page : $extra == 1,
            },
        )*
        ]
    }};
}

// Loopup table defined from here: http://www.oxyron.de/html/opcodes02.html

/// Table of opcodes. Columns are lower byte and rows are higher byte in this format. The machine
/// code interpreted directly as an index indexes into the array.
#[rustfmt::skip]
const OPCODE_LUT : [OpCode; 16*16] = format_lut!(
    (BRK,XXX,7,0),(ORA,IZX,6,0),(KIL,XXX,0,0),(SLO,IZX,8,0),(NOP,ZP0,3,0),(ORA,ZP0,3,0),(ASL,ZP0,5,0),(SLO,ZP0,5,0),(PHP,XXX,3,0),(ORA,IMM,2,0),(ASL,XXX,2,0),(ANC,IMM,2,0),(NOP,ABS,4,0),(ORA,ABS,4,0),(ASL,ABS,6,0),(SLO,ABS,6,0),
    (BPL,REL,2,1),(ORA,IZY,5,1),(KIL,XXX,0,0),(SLO,IZY,8,0),(NOP,ZPX,4,0),(ORA,ZPX,4,0),(ASL,ZPX,6,0),(SLO,ZPX,6,0),(CLC,XXX,2,0),(ORA,ABY,4,1),(NOP,XXX,2,0),(SLO,ABY,7,0),(NOP,ABX,4,1),(ORA,ABX,4,1),(ASL,ABX,7,0),(SLO,ABX,7,0),
    (JSR,ABS,6,0),(AND,IZX,6,0),(KIL,XXX,0,0),(RLA,IZX,8,0),(BIT,ZP0,3,0),(AND,ZP0,3,0),(ROL,ZP0,5,0),(RLA,ZP0,5,0),(PLP,XXX,4,0),(AND,IMM,2,0),(ROL,XXX,2,0),(ANC,IMM,2,0),(BIT,ABS,4,0),(AND,ABS,4,0),(ROL,ABS,6,0),(RLA,ABS,6,0),
    (BMI,REL,2,1),(AND,IZY,5,1),(KIL,XXX,0,0),(RLA,IZY,8,0),(NOP,ZPX,4,0),(AND,ZPX,4,0),(ROL,ZPX,6,0),(RLA,ZPX,6,0),(SEC,XXX,2,0),(AND,ABY,4,1),(NOP,XXX,2,0),(RLA,ABY,7,0),(NOP,ABX,4,1),(AND,ABX,4,1),(ROL,ABX,7,0),(RLA,ABX,7,0),
    (RTI,XXX,6,0),(EOR,IZX,6,0),(KIL,XXX,0,0),(SRE,IZX,8,0),(NOP,ZP0,3,0),(EOR,ZP0,3,0),(LSR,ZP0,5,0),(SRE,ZP0,5,0),(PHA,XXX,3,0),(EOR,IMM,2,0),(LSR,XXX,2,0),(ALR,IMM,2,0),(JMP,ABS,3,0),(EOR,ABS,4,0),(LSR,ABS,6,0),(SRE,ABS,6,0),
    (BVC,REL,2,1),(EOR,IZY,5,1),(KIL,XXX,0,0),(SRE,IZY,8,0),(NOP,ZPX,4,0),(EOR,ZPX,4,0),(LSR,ZPX,6,0),(SRE,ZPX,6,0),(CLI,XXX,2,0),(EOR,ABY,4,1),(NOP,XXX,2,0),(SRE,ABY,7,0),(NOP,ABX,4,1),(EOR,ABX,4,1),(LSR,ABX,7,0),(SRE,ABX,7,0),
    (RTS,XXX,6,0),(ADC,IZX,6,0),(KIL,XXX,0,0),(RRA,IZX,8,0),(NOP,ZP0,3,0),(ADC,ZP0,3,0),(ROR,ZP0,5,0),(RRA,ZP0,5,0),(PLA,XXX,4,0),(ADC,IMM,2,0),(ROR,XXX,2,0),(ARR,IMM,2,0),(JMP,IND,5,0),(ADC,ABS,4,0),(ROR,ABS,6,0),(RRA,ABS,6,0),
    (BVS,REL,2,1),(ADC,IZY,5,1),(KIL,XXX,0,0),(RRA,IZY,8,0),(NOP,ZPX,4,0),(ADC,ZPX,4,0),(ROR,ZPX,6,0),(RRA,ZPX,6,0),(SEI,XXX,2,0),(ADC,ABY,4,1),(NOP,XXX,2,0),(RRA,ABY,7,0),(NOP,ABX,4,1),(ADC,ABX,4,1),(ROR,ABX,7,0),(RRA,ABX,7,0),
    (NOP,IMM,2,0),(STA,IZX,6,0),(NOP,IMM,2,0),(SAX,IZX,6,0),(STY,ZP0,3,0),(STA,ZP0,3,0),(STX,ZP0,3,0),(SAX,ZP0,3,0),(DEY,XXX,2,0),(NOP,IMM,2,0),(TXA,XXX,2,0),(XAA,IMM,2,0),(STY,ABS,4,0),(STA,ABS,4,0),(STX,ABS,4,0),(SAX,ABS,4,0),
    (BCC,REL,2,1),(STA,IZY,6,0),(KIL,XXX,0,0),(AHX,IZY,6,0),(STY,ZPX,4,0),(STA,ZPX,4,0),(STX,ZPY,4,0),(SAX,ZPY,4,0),(TYA,XXX,2,0),(STA,ABY,5,0),(TXS,XXX,2,0),(TAS,ABY,5,0),(SHY,ABX,5,0),(STA,ABX,5,0),(SHX,ABY,5,0),(AHX,ABY,5,0),
    (LDY,IMM,2,0),(LDA,IZX,6,0),(LDX,IMM,2,0),(LAX,IZX,6,0),(LDY,ZP0,3,0),(LDA,ZP0,3,0),(LDX,ZP0,3,0),(LAX,ZP0,3,0),(TAY,XXX,2,0),(LDA,IMM,2,0),(TAX,XXX,2,0),(LAX,IMM,2,0),(LDY,ABS,4,0),(LDA,ABS,4,0),(LDX,ABS,4,0),(LAX,ABS,4,0),
    (BCS,REL,2,1),(LDA,IZY,5,1),(KIL,XXX,0,0),(LAX,IZY,5,1),(LDY,ZPX,4,0),(LDA,ZPX,4,0),(LDX,ZPY,4,0),(LAX,ZPY,4,0),(CLV,XXX,2,0),(LDA,ABY,4,1),(TSX,XXX,2,0),(LAS,ABY,4,1),(LDY,ABX,4,1),(LDA,ABX,4,1),(LDX,ABY,4,1),(LAX,ABY,4,1),
    (CPY,IMM,2,0),(CMP,IZX,6,0),(NOP,IMM,2,0),(DCP,IZX,8,0),(CPY,ZP0,3,0),(CMP,ZP0,3,0),(DEC,ZP0,5,0),(DCP,ZP0,5,0),(INY,XXX,2,0),(CMP,IMM,2,0),(DEX,XXX,2,0),(AXS,IMM,2,0),(CPY,ABS,4,0),(CMP,ABS,4,0),(DEC,ABS,6,0),(DCP,ABS,6,0),
    (BNE,REL,2,1),(CMP,IZY,5,1),(KIL,XXX,0,0),(DCP,IZY,8,0),(NOP,ZPX,4,0),(CMP,ZPX,4,0),(DEC,ZPX,6,0),(DCP,ZPX,6,0),(CLD,XXX,2,0),(CMP,ABY,4,1),(NOP,XXX,2,0),(DCP,ABY,7,0),(NOP,ABX,4,1),(CMP,ABX,4,1),(DEC,ABX,7,0),(DCP,ABX,7,0),
    (CPX,IMM,2,0),(SBC,IZX,6,0),(NOP,IMM,2,0),(ISC,IZX,8,0),(CPX,ZP0,3,0),(SBC,ZP0,3,0),(INC,ZP0,5,0),(ISC,ZP0,5,0),(INX,XXX,2,0),(SBC,IMM,2,0),(NOP,XXX,2,0),(SBC,IMM,2,0),(CPX,ABS,4,0),(SBC,ABS,4,0),(INC,ABS,6,0),(ISC,ABS,6,0),
    (BEQ,REL,2,1),(SBC,IZY,5,1),(KIL,XXX,0,0),(ISC,IZY,8,0),(NOP,ZPX,4,0),(SBC,ZPX,4,0),(INC,ZPX,6,0),(ISC,ZPX,6,0),(SED,XXX,2,0),(SBC,ABY,4,1),(NOP,XXX,2,0),(ISC,ABY,7,0),(NOP,ABX,4,1),(SBC,ABX,4,1),(INC,ABX,7,0),(ISC,ABX,7,0)
);

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_machine_code() {
        assert_eq!(OPCODE_LUT[0].machine_code, 0);
        assert_eq!(OPCODE_LUT[1].machine_code, 1);
    }

    #[test]
    fn test_lookup_table() {
        // Get random value from table
        let op = OpCode::get(0x01);
        assert_eq!(op.name, "ORA");
        assert_eq!(op.instruction, Instruction::ORA);
        assert_eq!(op.mode, AddressingMode::IZX);
        assert_eq!(op.cycles, 6);
        assert_eq!(op.add_cycle_for_new_page, false);

        // Get another random value from the table
        let op = OpCode::get(0xF1);
        assert_eq!(op.name, "SBC");
        assert_eq!(op.instruction, Instruction::SBC);
        assert_eq!(op.mode, AddressingMode::IZY);
        assert_eq!(op.cycles, 5);
        assert_eq!(op.add_cycle_for_new_page, true);

        // Sanity check
        assert_eq!(u8::MAX, 0xFF);

        // Get last element in table.
        let op = OpCode::get(u8::MAX);
        assert_eq!(op.name, "ISC");
        assert_eq!(op.instruction, Instruction::ISC);
        assert_eq!(op.mode, AddressingMode::ABX);
        assert_eq!(op.cycles, 7);
        assert_eq!(op.add_cycle_for_new_page, false);

        assert_eq!(OPCODE_LUT.len(), 16 * 16)
    }
}
