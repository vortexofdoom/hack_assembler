use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use once_cell::sync::Lazy;
use bitflags::bitflags;

static BUILTIN_LABELS: Lazy<HashMap<&'static str, u16>> = Lazy::new(|| {
    let mut labels = HashMap::new();
        labels.insert("SP", 0);
        labels.insert("LCL", 1);
        labels.insert("ARG", 2);
        labels.insert("THIS", 3);
        labels.insert("THAT", 4);
        labels.insert("R0", 0);
        labels.insert("R1", 1);
        labels.insert("R2", 2);
        labels.insert("R3", 3);
        labels.insert("R4", 4);
        labels.insert("R5", 5);
        labels.insert("R6", 6);
        labels.insert("R7", 7);
        labels.insert("R8", 8);
        labels.insert("R9", 9);
        labels.insert("R10", 10);
        labels.insert("R11", 11);
        labels.insert("R12", 12);
        labels.insert("R13", 13);
        labels.insert("R14", 14);
        labels.insert("R15", 15);
        labels.insert("SCREEN", 16384);
        labels.insert("KBD", 24576);
        labels
});

bitflags! {
    struct AsmFlags: u16 {
        // Jump bits
        const JGT       = 1;        // Jump if greater than
        const JEQ       = 1 << 1;   // Jump if equal to
        const JLT       = 1 << 2;   // jump if less than

        // Destination bits
        const DEST_M     = 1 << 3;   // Store in M register (Address specified by A register)
        const DEST_D     = 1 << 4;   // Store in D register
        const DEST_A     = 1 << 5;   // Store in A register

        // Control bits
        // These are semantically named based on the ALU chip of the CPU
        const CTRL_NOT     = 1 << 6;
        const CTRL_ADD     = 1 << 7;
        const CTRL_NA      = 1 << 8;
        const CTRL_ZA      = 1 << 9;
        const CTRL_ND      = 1 << 10;
        const CTRL_ZD      = 1 << 11;
        const ADDR      = 1 << 12;  // A as Address vs value (M vs A)

        // C instruction bits
        // For C instructions these are always set in the Hack specification, but never used within the Hack architecture
        // We define all of them so that we can use this under the hood for both A and C instructions
        const B0        = 1 << 13;
        const B1        = 1 << 14;
        const C         = 1 << 15;

        // Combined flags for J and D bits, since they are easily enumerable and have intuitive semantics
        // Destination combinations might not be needed, but can revisit that later
        const JGE       = 0b011;    // Jump if greater than or equal to
        const JNE       = 0b101;    // Jump if not equal to
        const JLE       = 0b110;    // Jump if less than or equal to
        const JMP       = 0b111;    // Unconditional Jump        
        const DEST_MD    = 0b011 << 3;
        const DEST_AM    = 0b101 << 3;
        const DEST_AD    = 0b110 << 3;
        const DEST_AMD   = 0b111 << 3;
        
        // Relevant c-bit combinations for the assembly language. Comments are how they literally resolve into ALU output
        // 0 + 0      => 0
        const CTRL_0         = Self::CTRL_ADD.bits() | Self::CTRL_ZA.bits() | Self::CTRL_ZD.bits();   
        
        // !(-1 + -1) => 1
        const CTRL_1         = Self::CTRL_NOT.bits() | Self::CTRL_ADD.bits() | Self::CTRL_NA.bits() | Self::CTRL_ZA.bits() | Self::CTRL_ND.bits() | Self::CTRL_ZD.bits();
        
        // -1 + 0     => -1
        const CTRL_NEG_1     = Self::CTRL_0.bits() | Self::CTRL_ND.bits();
        
        // D & -1     => D
        const CTRL_D         = Self::CTRL_NA.bits() | Self::CTRL_ZA.bits();
        
        // -1 & AM    => A
        const CTRL_A        = Self::CTRL_ND.bits() | Self::CTRL_ZD.bits();
        
        // !(D & -1)  => !D
        const CTRL_NOT_D     = Self::CTRL_D.bits() | Self::CTRL_NOT.bits();
        
        // !(-1 & A)  => !A
        const CTRL_NOT_A     = Self::CTRL_A.bits() | Self::CTRL_NOT.bits();
        
        // !(D + -1)  => -D
        const CTRL_NEG_D     = Self::CTRL_NOT_D.bits() | Self::CTRL_ADD.bits();
        
        // !(-1 + A)  => -A
        const CTRL_NEG_A     = Self::CTRL_NOT_A.bits() | Self::CTRL_ADD.bits();
        
        // !(!D + -1) => D + 1
        const CTRL_D_PLUS_1  = Self::CTRL_1.bits() & !Self::CTRL_ZD.bits();
        
        // !(-1 + !A) => A + 1
        const CTRL_A_PLUS_1  = Self::CTRL_1.bits() & !Self::CTRL_ZA.bits();
        
        // (D + -1)     => (D - 1)
        const CTRL_D_MINUS_1 = Self::CTRL_ADD.bits() | Self::CTRL_NA.bits() | Self::CTRL_ZA.bits();
        
        // (-1 + A)     => (A - 1)
        const CTRL_A_MINUS_1 = Self::CTRL_ADD.bits() | Self::CTRL_ND.bits() | Self::CTRL_ZD.bits();
        
        // !(!D + A)  => (D - A)
        const CTRL_D_SUB_A   = Self::CTRL_ND.bits() | Self::CTRL_ADD.bits() | Self::CTRL_NOT.bits();
        
        // !(D + !A)  => (A - D)
        const CTRL_A_SUB_D   = Self::CTRL_NOT.bits() | Self::CTRL_ADD.bits() | Self::CTRL_NA.bits();
        
        // !(!D & !A) => (D | A)
        const CTRL_D_OR_A    = Self::CTRL_NOT.bits() | Self::CTRL_NA.bits() | Self::CTRL_ND.bits();

        // // 
        // const CMP_M         = Self::CMP_A.bits() | Self::ADDR.bits();
        // const CMP_NOT_M     = Self::CMP_NOT_A.bits() | Self::ADDR.bits();
        // const CMP_NEG_M     = Self::CMP_NEG_A.bits() | Self::ADDR.bits();
        // const CMP_M_PLUS_1  = Self::CMP_A_PLUS_1.bits() | Self::ADDR.bits();
        // const CMP_M_MINUS_1 = Self::CMP_
    }
}

enum Dest {
    M,
    D,
    MD,
    A,
    AM,
    AD,
    AMD,
}

enum Jump {
    JGT,
    JEQ,
    JGE,
    JLT,
    JNE,
    JLE,
    JMP,
}

enum Ctrl {
    Zero,
    One,
    NegOne,
    D,
    A,
    M,
    NotD,
    NotA,
    NotM,
    NegD,
    NegA,
    NegM,
    DPlus1,
    APlus1,
    MPlus1,
    DMinus1,
    AMinus1,
    MMinus1,
    DPlusA,
    DPlusM,
    DMinusA,
    DMinusM,
    AMinusD,
    MMinusD,
    DAndA,
    DAndM,
    DOrA,
    DOrM,
}

struct Instruction {
    inner: AsmFlags,
}

impl From<u16> for Instruction {
    fn from(value: u16) -> Self {
        todo!()
    }
}

impl Instruction {
    pub fn dest(&self) -> Option<Dest> {
        match self.inner.intersection(AsmFlags::DEST_AMD) {
            AsmFlags::DEST_A => Some(Dest::A),
            AsmFlags::DEST_M => Some(Dest::M),
            AsmFlags::DEST_D => Some(Dest::D),
            AsmFlags::DEST_AM => Some(Dest::AM),
            AsmFlags::DEST_AD => Some(Dest::AD),
            AsmFlags::DEST_MD => Some(Dest::MD),
            AsmFlags::DEST_AMD => Some(Dest::AMD),
            _ => None,
        }
    }

    pub fn jump(&self) -> Option<Jump> {
        match self.inner.intersection(AsmFlags::JMP) {
            AsmFlags::JGT => Some(Jump::JGT),
            AsmFlags::JGE => Some(Jump::JGE),
            AsmFlags::JEQ => Some(Jump::JEQ),
            AsmFlags::JLT => Some(Jump::JLT),
            AsmFlags::JLE => Some(Jump::JLE),
            AsmFlags::JNE => Some(Jump::JNE),
            _ => None,
        }
    }

    pub fn ctrl(&self) -> Ctrl {
        match (self.inner.intersection(AsmFlags::CTRL_1), self.contains(AsmFlags::ADDR)) {
            // Official
            (AsmFlags:: )
        }
    }
}

impl std::fmt::Display for Instruction {
    
}

impl std::fmt::Display for AsmFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let jmp = match self.intersection(Self::JMP) {
            Self::JGT => ";JGT",
            Self::JEQ => ";JEQ",
            Self::JGE => ";JGE",
            Self::JLT => ";JLT",
            Self::JNE => ";JNE",
            Self::JLE => ";JLE",
            Self::JMP => ";JMP",
            _ => "",    // if no flags, no jump instruction
        };

        let dest = match self.intersection(Self::DEST_AMD) {
            Self::DEST_M    => "M=",
            Self::DEST_D    => "D=",
            Self::DEST_MD   => "MD=",
            Self::DEST_A    => "A=",
            Self::DEST_AM   => "AM=",
            Self::DEST_AD   => "AD=",
            Self::DEST_AMD  => "AMD=",
            _ => "",
        };

        let var_a = if self.contains(Self::ADDR) {
            'M'
        } else {
            'A'
        };

    /* 
        
       CTRL a(c c c c c c)
      a = 0 | 5 4 3 2 1 0 | a = 1
          0 | 1 0 1 0 1 0 |
          1 | 1 1 1 1 1 1 |
         -1 | 1 1 1 0 1 0 |
          D | 0 0 1 1 0 0 |
          A | 1 1 0 0 0 0 | M
         !D | 0 0 1 1 0 1 |
         !A | 1 1 0 0 0 1 | !M
         -D | 0 0 1 1 1 1 |
         -A | 1 1 0 0 1 1 | -M
        D+1 | 0 1 1 1 1 1 |
        A+1 | 1 1 0 1 1 1 | M+1
        D-1 | 0 0 1 1 1 0 |
        A-1 | 1 1 0 0 1 0 | M-1
        D+A | 0 0 0 0 1 0 | D+M
        D-A | 0 1 0 0 1 1 | D-M
        A-D | 0 0 0 1 1 1 | M-D
        D&A | 0 0 0 0 0 0 | D&M
        D|A | 0 1 0 1 0 1 | D|M
        */
        let cmp = match self.intersection(Self::CTRL_1 | Self::ADDR) {
            Self::CTRL_0 => "0",
            Self::CTRL_1 => "1",
            Self::CTRL_NEG_1 => "-1",
            Self::CTRL_D => "D",
            Self::CTRL_A => "{var_a}",
            Self::CTRL_NOT_D => "!D",
            Self::CTRL_NOT_A => "!A",
            Self::CTRL_NEG_D => "-D",
            Self::CTRL_NEG_A => "-{var_a}",

            0b110001 => "!{var_a}",
            0b001111 => "-D",
            0b110011 => "-{var_a}",
            0b011111 => ""

        };
    }
}

#[repr(u16)]
enum InstructionType {
    A = 0,
    C = 1,
}

impl From<u16> for InstructionType {
    fn from(value: u16) -> Self {
        if value >> 15 == 0 {
            Self::A
        } else {
            Self::C
        }
    }
}

// impl std::str::FromStr for AsmFlags {
//     type Err = String; // TODO: Better error

//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         if s.starts_with('@') {

//         }
//     }
// }

impl Default for AsmFlags {
    fn default() -> Self {
        Self::C | Self::B1 | Self::B0 
    }
}

struct Assembler {
    pub labels: HashMap<String, u16>,
    pub var_counter: u16,
}

impl Assembler {
    pub fn new() -> Self {
        Assembler {
            labels: HashMap::new(),
            var_counter: 16,
        }
    }

    // Helper function to abstract over checking the static list first, then the labels unique to this assembly
    fn get_label(&self, label: &str) -> Option<&u16> {
        if let Some(i) = BUILTIN_LABELS.get(label) {
            Some(i)
        } else if let Some(i) = self.labels.get(label) {
            Some(i)
        } else {
            None
        }
    }

    pub fn translate(&mut self, input: &str) -> u16 {
        let mut inst = 0u16;
        // A or C instruction
        if input.starts_with('@') {
            match input[1..].parse::<i16>() {
                Ok(n) if n > 0 => inst = n as u16,
                Err(_) => {
                    if let Some(&addr) = self.get_label(&input[1..]) {
                        return addr;
                    } else {
                        self.labels.insert(String::from(&input[1..]), self.var_counter);
                        self.var_counter += 1;
                        return self.var_counter - 1;
                    }
                }
                _ => panic!("Can only address positive signed 16bit integers")
            }
        } else {
            let mut inst = AsmFlags::default();
    
            let mut comp_start = 0;
            let mut comp_end = input.len();

            // DEST (ddd)
            if let Some(i) = input.find('=') {
                comp_start = i + 1;
                let dest = &input[..i];
                inst.set(AsmFlags::DEST_M, dest.contains('M'));
                inst.set(AsmFlags::DEST_D, dest.contains('D'));
                inst.set(AsmFlags::DEST_A, dest.contains('A'));
            }
            // JUMP (jjj)
            if let Some(i) = input.find(';') {
                comp_end = i;
                let jump = &input[i+1..];
                match jump {
                    "JGT" => inst |= AsmFlags::JGT,
                    "JEQ" => inst |= AsmFlags::JEQ,
                    "JGE" => inst |= AsmFlags::JGE,
                    "JLT" => inst |= AsmFlags::JLT,
                    "JNE" => inst |= AsmFlags::JNE,
                    "JLE" => inst |= AsmFlags::JLE,
                    "JMP" => inst |= AsmFlags::JMP,
                    _ => panic!("Semicolon requires a jump command!")
                }
            }
    
            /* 
            
          COMP  a(c c c c c c)
          a = 0 | 5 4 3 2 1 0 | a = 1
              0 | 1 0 1 0 1 0 |
              1 | 1 1 1 1 1 1 |
             -1 | 1 1 1 0 1 0 |
              D | 0 0 1 1 0 0 |
              A | 1 1 0 0 0 0 | M
             !D | 0 0 1 1 0 1 |
             !A | 1 1 0 0 0 1 | !M
             -D | 0 0 1 1 1 1 |
             -A | 1 1 0 0 1 1 | -M
            D+1 | 0 1 1 1 1 1 |
            A+1 | 1 1 0 1 1 1 | M+1
            D-1 | 0 0 1 1 1 0 |
            A-1 | 1 1 0 0 1 0 | M-1
            D+A | 0 0 0 0 1 0 | D+M
            D-A | 0 1 0 0 1 1 | D-M
            A-D | 0 0 0 1 1 1 | M-D
            D&A | 0 0 0 0 0 0 | D&M
            D|A | 0 1 0 1 0 1 | D|M
            */
             
            let comp = &input[comp_start..comp_end];
            let mut c_bits = 0b000000;
            if comp.contains('M') { // all M computations have 'a' bit set
                inst |= 1 << 12;
            }
            if comp == "0" {                    // 0
                c_bits = 0b101010;
            } else if comp == "1" {             // 1
                c_bits = 0b111111;
            } else if comp == "-1" {            // -1
                c_bits = 0b111010;
            } else if comp.len() <= 2 {
                if comp.contains('D') {         // D
                    c_bits = 0b001100;
                } else {                        // A / M
                    c_bits = 0b110000;
                }
                if comp.contains('!') {         // !D / !A / !M
                    c_bits |= 0b000001;
                } else if comp.contains('-') {  // -D / -A / -M
                    c_bits |= 0b000011;
                }
            } else if comp.contains("+1") {
                c_bits = 0b000111;
                // D+1 | A+1/M+1
                //c_bits |= 0b11 << (3 + comp.contains("D") as u8);
                if comp.contains('D') {         // D+1
                    c_bits |= 0b011000;
                } else {                        // A+1 / M+1
                    c_bits |= 0b110000;
                }
            } else if comp.contains('D') && comp.contains('+') {     // D+A / D+M
                c_bits = 0b000010;
            } else if comp == "D-1" {           // D-1
                c_bits = 0b001110;
            } else if comp.contains("-1") {     // A-1 / M-1
                c_bits = 0b110010;
            } else if comp.contains("D-") {     // D-A / D-M
                c_bits = 0b010011;
            } else if comp.contains('-') {      // A-D / M-D
                c_bits = 0b000111;
            } else if comp.contains('|') {      // D|A / D|M
                c_bits = 0b010101;
            } // Don't need anything for & since it's already 0
            inst |= c_bits << 6;
        }
        inst
    }

    pub fn assemble(&mut self, asm: &[String]) -> Vec<u16> {
        // first pass
        let mut line: u16 = 0;
        for com in asm {
            if let (Some('('), Some(')')) = (com.chars().nth(0), com.chars().nth_back(0)) {
                //println!("{com}");
                let label = &com[1..com.len() - 1];
                if self.get_label(label).is_none() {
                    self.labels.insert(String::from(label), line);
                }
            } else {
                line += 1;
            }
        }
        asm
            .iter()
            .filter(|&c| !c.contains("("))
            .map(|c| self.translate(&c))
            .collect()
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let filename = args[1].clone();
    let mut asm = vec![];
    let mut assembler = Assembler::new();
    if let Ok(f) = File::open(filename) {
        let reader = BufReader::new(f);
        for line in reader.lines() {
            if let Ok(s) = line {
                let cmd = strip_line(&s);
                if !cmd.is_empty() {
                    asm.push(cmd);
                }
            }
        }
    }
    let bin = assembler.assemble(&asm);
    for b in bin {
        println!("{}", format!("{b:016b}"));
    }
}

fn strip_line(input: &str) -> String {
    input
        .find("//")
        .map(|i| &input[..i])
        .unwrap_or(input)
        .replace(' ', "")
}


 