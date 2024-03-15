use crate::consts::KILOBYTE;

pub struct Header {
    bytes: [u8; 16],
}

impl Header {
    fn new(bytes: [u8; 16]) -> Header {
        let bytes = bytes.clone();
        Header { bytes }
    }

    fn submapper(&self) -> u16 {
        let m = self.bytes[8] as u16 & 0xF0;

        let mapper = m >> 4;
        mapper
    }

    fn mapper(&self) -> u16 {
        let m1 = self.bytes[6] as u16 & 0xF0;
        let m2 = self.bytes[7] as u16 & 0xF0;
        let m3 = self.bytes[8] as u16 & 0x0F;

        let mapper = m1 >> 4 | m2 | m3 << 8;
        mapper
    }

    fn has_trainer(&self) -> bool {
        self.bytes[8] & 0b100 != 0
    }

    fn is_valid(&self) -> bool {
        let is_ines_format = self.bytes[0] == b'N'
            && self.bytes[1] == b'E'
            && self.bytes[2] == b'S'
            && self.bytes[3] == 0x1A;
        let _is_ines_2_format = is_ines_format && self.bytes[7] & 0b00001000 != 0;
        is_ines_format
    }

    // Funky size calculation for nes files
    fn get_rom_size(lsb: usize, msb: usize) -> usize {
        if msb == 0x0F {
            let mult = lsb & 0b11;
            let exp = lsb >> 2;
            (1 << exp) * (mult * 2 + 1)
        } else {
            lsb | (msb << 12)
        }
    }

    fn prg_rom_size(&self) -> usize {
        let prg_rom_lsb = self.bytes[4] as usize;
        let prg_rom_msb = (self.bytes[9] & 0x0F) as usize;
        let prg_rom_size_kb = Self::get_rom_size(prg_rom_lsb, prg_rom_msb);
        prg_rom_size_kb * 16 * KILOBYTE
    }

    fn chr_rom_size(&self) -> usize {
        let chr_rom_lsb = self.bytes[5] as usize;
        let chr_rom_msb = ((self.bytes[9] & 0xF0) >> 4) as usize;
        let chr_rom_size_kb = Self::get_rom_size(chr_rom_lsb, chr_rom_msb);
        chr_rom_size_kb * 8 * KILOBYTE
    }
}

pub struct Trainer {
    bytes: [u8; 512],
}
impl Trainer {
    fn new(bytes: [u8; 512]) -> Self {
        Self { bytes }
    }
}
// https://www.nesdev.org/wiki/NES_2.0#PRG-ROM_Area
pub struct NesFile {
    header: Header,
    trainer: Option<Trainer>,
    // Program ROM. Connected to CPU
    prg_rom: Vec<u8>,
    // Character ROM. Connected to PPU
    chr_rom: Vec<u8>,
    misc_rom: Vec<u8>,
}
impl NesFile {
    pub fn print_info(&self) {
        println!("Mapper       : {}", self.header.mapper());
        println!("Submapper    : {}", self.header.submapper());
        println!("Has trainer?   {}", self.header.has_trainer());
        println!("CHR ROM SIZE:  {}", self.chr_rom.len());
        println!("PRG ROM SIZE:  {}", self.prg_rom.len());
        println!("Misc ROM SIZE: {}", self.misc_rom.len());
    }

    pub fn chr_rom(&self) -> &[u8] {
        &self.chr_rom
    }

    pub fn prg_rom(&self) -> &[u8] {
        &self.prg_rom
    }

    pub fn try_from_file<P>(file_name: P) -> Self
    where
        P: AsRef<std::path::Path>,
    {
        let bytes = std::fs::read(file_name).unwrap();
        // Get header
        let header = Header::new(bytes[0..16].try_into().unwrap());
        // Pointer to current reading location
        let mut seek = 16;
        if !header.is_valid() {
            panic!("Bad .nes file");
        }

        // Get trainer
        let mut trainer = None;
        if header.has_trainer() {
            trainer = Some(Trainer::new(bytes[seek..seek + 512].try_into().unwrap()));
            seek += 512;
        }

        // Get PRG ROM
        let prg_rom_size = header.prg_rom_size();
        let prg_rom = bytes[seek..seek + prg_rom_size].to_vec();
        seek += prg_rom_size;

        // Get CHR ROM
        let chr_rom_size = header.chr_rom_size();
        let chr_rom = bytes[seek..seek + chr_rom_size].to_vec();
        seek += chr_rom_size;

        // Get MISC ROM
        let misc_rom = bytes[seek..].to_vec();
        NesFile {
            header,
            trainer,
            prg_rom,
            chr_rom,
            misc_rom,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_nes_file() {
        let basename = "./rom_tests/nes-test-roms/";
        let files = [
            "cpu_dummy_reads/cpu_dummy_reads.nes",
            "instr_test-v5/rom_singles/01-basics.nes",
            "instr_test-v5/official_only.nes",
            "nes_instr_test/rom_singles/01-implied.nes",
            //"nes_instr_test/rom_singles/02-immediate.nes",
            //"nes_instr_test/rom_singles/03-zero_page.nes",
            //"nes_instr_test/rom_singles/04-zp_xy.nes",
            //"nes_instr_test/rom_singles/05-absolute.nes",
            //"nes_instr_test/rom_singles/06-abs_xy.nes",
            //"nes_instr_test/rom_singles/07-ind_x.nes",
            //"nes_instr_test/rom_singles/08-ind_y.nes",
            //"nes_instr_test/rom_singles/09-branches.nes",
            //"nes_instr_test/rom_singles/10-stack.nes",
            //"nes_instr_test/rom_singles/11-special.nes",
        ];

        for file in files {
            let file_name = basename.to_string() + file;
            println!("reading rom = {file_name}");
            let nes_file = NesFile::try_from_file(&file_name);
            nes_file.print_info();
            assert!(nes_file.prg_rom().len() > 0);
        }
        //panic!()
    }
}
