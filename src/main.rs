use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

struct Assembler {
    pub labels: HashMap<String, u16>,
    pub var_counter: u16,
}

impl Assembler {
    pub fn new() -> Self {
        let mut labels = HashMap::new();
        labels.insert("SP".to_string(), 0);
        labels.insert("LCL".to_string(), 1);
        labels.insert("ARG".to_string(), 2);
        labels.insert("THIS".to_string(), 3);
        labels.insert("THAT".to_string(), 4);
        labels.insert("R0".to_string(), 0);
        labels.insert("R1".to_string(), 1);
        labels.insert("R2".to_string(), 2);
        labels.insert("R3".to_string(), 3);
        labels.insert("R4".to_string(), 4);
        labels.insert("R5".to_string(), 5);
        labels.insert("R6".to_string(), 6);
        labels.insert("R7".to_string(), 7);
        labels.insert("R8".to_string(), 8);
        labels.insert("R9".to_string(), 9);
        labels.insert("R10".to_string(), 10);
        labels.insert("R11".to_string(), 11);
        labels.insert("R12".to_string(), 12);
        labels.insert("R13".to_string(), 13);
        labels.insert("R14".to_string(), 14);
        labels.insert("R15".to_string(), 15);
        labels.insert("SCREEN".to_string(), 16384);
        labels.insert("KBD".to_string(), 24576);
        
        Assembler {
            labels: labels,
            var_counter: 16,
        }
    }

    pub fn translate(&mut self, input: &str) -> u16 {
        let mut inst = 0u16;
        // A or C instruction
        if let Some('@') = input.chars().nth(0) {
            match input[1..].parse::<u16>() {
                Ok(n) => inst = n,
                Err(_) => {
                    if let Some(&addr) = self.labels.get(&input[1..]) {
                        inst = addr;
                    } else {
                        self.labels.insert(String::from(&input[1..]), self.var_counter);
                        inst = self.var_counter;
                        self.var_counter += 1;
                    }
                }
            }
        } else {
            //        op// a cccccc ddd jjj
            inst |= 0b1_11_0_000000_000_000;
    
            let mut comp_start = 0;
            let mut comp_end = input.len();
            if let Some(i) = input.find("=") {
                comp_start = i + 1;
                let dest = &input[..i];
                if dest.contains("M") {
                    inst |= 1 << 3;
                }
                if dest.contains("D") {
                    inst |= 1 << 4;
                }
                if dest.contains("A") {
                    inst |= 1 << 5;
                }
            }
            // JUMP
            if let Some(i) = input.find(";") {
                comp_end = i;
                let jump = &input[i+1..];
                match jump {
                    "JGT" => inst |= 0b001,
                    "JEQ" => inst |= 0b010,
                    "JGE" => inst |= 0b011,
                    "JLT" => inst |= 0b100,
                    "JNE" => inst |= 0b101,
                    "JLE" => inst |= 0b110,
                    "JMP" => inst |= 0b111,
                    _ => panic!("Semicolon requires a jump command!")
                }
            }
    
            // COMP
            let comp = &input[comp_start..comp_end];
            let mut c_bits = 0b000000;
            if comp.contains("M") { // all M computations have 'a' bit set
                inst |= 1 << 12;
            }
            if comp == "0" {                    // 0
                c_bits = 0b101010;
            } else if comp == "1" {             // 1
                c_bits = 0b111111;
            } else if comp == "-1" {            // -1
                c_bits = 0b111010;
            } else if comp.len() <= 2 {
                if comp.contains("D") {         // D
                    c_bits = 0b001100;
                } else {                        // A / M
                    c_bits = 0b110000;
                }
                if comp.contains("!") {         // !D / !A / !M
                    c_bits |= 0b000001;
                } else if comp.contains("-") {  // -D / -A / -M
                    c_bits |= 0b000011;
                }
            } else if comp.contains("+1") {
                c_bits = 0b000111;
                if comp.contains("D") {         // D+1
                    c_bits |= 0b011000;
                } else {                        // A+1 / M+1
                    c_bits |= 0b110000;
                }
            } else if comp.contains("D+") || comp.contains("+D") {     // D+A / D+M
                c_bits = 0b000010;
            } else if comp == "D-1" {           // D-1
                c_bits = 0b001110;
            } else if comp.contains("-1") {     // A-1 / M-1
                c_bits = 0b110010;
            } else if comp.contains("D-") {     // D-A / D-M
                c_bits = 0b010011;
            } else if comp.contains("-") {      // A-D / M-D
                c_bits = 0b000111;
            } else if comp.contains("&") {      // D&A / D&M
                c_bits = 0b000000;
            } else if comp.contains("|") {      // D|A / D|M
                c_bits = 0b010101;
            }
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
                self.labels.insert(String::from(&com[1..com.len() - 1]), line);
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
        .replace(" ", "")
}


 