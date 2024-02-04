use crate::consts::*;
// https://www.nesdev.org/wiki/NES_2.0#PRG-ROM_Area
pub struct NesFile {
    bytes: Vec<u8>,
    prg_rom: u8,
    prg_rom_size: u8,
    chr_rom: u8,
    chr_rom_size: u8,
}
impl NesFile {
    fn prg_rom_size(&self) -> usize {
        if self.prg_rom_size != 0 {
            panic!("PRG ROM SIZE MULTIPLIER NOT HANDLED");
        }
        (self.prg_rom as usize) * 16 * KILOBYTE
    }
    fn chr_rom_size(&self) -> usize {
        if self.chr_rom_size != 0 {
            panic!("CHR ROM SIZE MULTIPLIER NOT HANDLED");
        }
        (self.chr_rom as usize) * 8 * KILOBYTE
    }
    pub fn prg_rom(&self) -> Vec<u8> {
        let prg_rom_start = 16;
        let prg_rom_end = prg_rom_start + self.prg_rom_size();
        self.bytes[prg_rom_start..prg_rom_end].to_vec()
    }
    pub fn print_info(&self) {
        let prg_len = self.prg_rom_size();
        let chr_len = self.chr_rom_size();
        let prg_start = 16;
        let prg_end = prg_start + prg_len;
        let chr_start = prg_end + 1;
        let chr_end = chr_start + chr_len;
        println!("header 0x00..0xFF");
        println!("trainer 0xFF..0xFF");
        println!("prg = 0x{prg_start:.X}..0x{prg_end:.X}");
        println!("chr size = 0x{chr_start:.X}..0x{chr_end:.X}");
    }
    pub fn try_from_file<P>(file_name: P) -> Self
    where
        P: AsRef<std::path::Path>,
    {
        let bytes = std::fs::read(file_name).unwrap();
        let is_ines_format =
            bytes[0] == b'N' && bytes[1] == b'E' && bytes[2] == b'S' && bytes[3] == 0x1A;
        let _is_ines_2_format = is_ines_format && bytes[7] == 0x08;

        let prg_rom = bytes[4];
        let chr_rom = bytes[5];

        let prg_rom_size = bytes[9] >> 4;
        let chr_rom_size = bytes[9] & 0x0F;

        assert!(is_ines_format);
        Self {
            bytes,
            prg_rom,
            chr_rom,
            prg_rom_size,
            chr_rom_size,
        }
    }
}
