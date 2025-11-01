use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{self, Read, Seek, SeekFrom};
use std::process;

const MNEMO: [&str; 16] = [
    "MOV", "LSL", "ASR", "ROR",
    "AND", "ANN", "IOR", "XOR",
    "ADD", "SUB", "MUL", "DIV",
    "FAD", "FSB", "FML", "FDV",
];

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum BranchCondition {
    Always,
    Known(&'static str),
    Unknown(u32),
}

macro_rules! debug_println {
    ($enabled:expr, $($arg:tt)*) => {
        if $enabled {
            println!($($arg)*);
        }
    };
}

fn decode_condition(a: u32) -> BranchCondition {
    match a {
        7 => BranchCondition::Always,
        0 => BranchCondition::Known("MI"),
        8 => BranchCondition::Known("PL"),
        1 => BranchCondition::Known("EQ"),
        9 => BranchCondition::Known("NE"),
        2 => BranchCondition::Known("LS"),
        10 => BranchCondition::Known("HI"),
        5 => BranchCondition::Known("LT"),
        13 => BranchCondition::Known("GE"),
        6 => BranchCondition::Known("LE"),
        14 => BranchCondition::Known("GT"),
        15 => BranchCondition::Known("NO"),
        other => BranchCondition::Unknown(other),
    }
}

fn read_u32(file: &mut File) -> io::Result<u32> {
    let mut buf = [0u8; 4];
    file.read_exact(&mut buf)?;
    Ok(u32::from_le_bytes(buf))
}

fn read_i32(file: &mut File) -> io::Result<i32> {
    let mut buf = [0u8; 4];
    file.read_exact(&mut buf)?;
    Ok(i32::from_le_bytes(buf))
}

fn read_cstring(file: &mut File) -> io::Result<String> {
    let mut bytes = Vec::new();
    loop {
        let mut buf = [0u8; 1];
        file.read_exact(&mut buf)?;
        if buf[0] == 0 {
            break;
        }
        bytes.push(buf[0]);
    }
    Ok(String::from_utf8_lossy(&bytes).to_string())
}

fn decode_instruction(instr: u32) -> (u32, u32, u32, u32, u32, u32, Option<i32>, Option<i32>) {
    let k = (instr >> 30) & 0x3;
    let u = (instr >> 29) & 0x1;
    let a = (instr >> 24) & 0xF;
    let b = (instr >> 20) & 0xF;
    let op = (instr >> 16) & 0xF;
    let c = instr & 0xF;

    let mut imm_signed = None;
    let mut disp_signed = None;

    if k == 1 {
        let imm = instr & 0xFFFF;
        imm_signed = Some(if imm & 0x8000 != 0 {
            (imm as i32) - 0x10000
        } else {
            imm as i32
        });
    } else if k == 2 || k == 3 {
        let disp = instr & 0xFFFFF;
        disp_signed = Some(if disp & 0x80000 != 0 {
            (disp as i32) - 0x100000
        } else {
            disp as i32
        });
    }

    (k, u, a, b, op, c, imm_signed, disp_signed)
}

fn translate_instruction(instr: u32, fixup_map: &HashMap<usize, String>, word_offset: usize) -> String {
    let (k, u, a, b, op, c, imm_signed, disp_signed) = decode_instruction(instr);

    let fix_target = fixup_map.get(&word_offset).map(|s| s.as_str());
    let annotation = fix_target
        .map(|s| format!(" -> {}", s))
        .unwrap_or_default();
    let cond = decode_condition(a);
    let cond_text = match cond {
        BranchCondition::Known(name) => name,
        BranchCondition::Always => "",
        BranchCondition::Unknown(_) => "??",
    };

    // Create RISC5 comment
    let mut risc5 = if k == 1 {
        format!("{} R{}, R{}, #{}", MNEMO[op as usize], a, b, imm_signed.unwrap())
    } else if k == 2 {
        if u == 1 {
            format!("STR R{}, R{}, {}", a, b, disp_signed.unwrap())
        } else {
            format!("LDR R{}, R{}, {}", a, b, disp_signed.unwrap())
        }
    } else if k == 3 {
        let link = if (instr >> 28) & 0x1 != 0 { "L" } else { "" };
        if u == 0 {
            format!("B{}{}R{}", link, cond_text, c)
        } else {
            format!("B{}{} {}", link, cond_text, disp_signed.unwrap())
        }
    } else {
        let u_mark = if u == 1 { "'" } else { "" };
        format!("{}{} R{}, R{}, R{}", MNEMO[op as usize], u_mark, a, b, c)
    };

    if k == 3 && u == 1 {
        if let Some(disp) = disp_signed {
            if fix_target.is_none() {
                let target_line = word_offset as i64 + 1 + disp as i64;
                risc5.push_str(&format!(" (-> line {})", target_line));
            }
        }
    }

    // AArch64 translation
    let aarch64 = if k == 0 {
        if u == 1 {
            format!("\t// TODO: k=0 u=1 form")
        } else if op == 0 {
            format!("\tmov\tx{}, x{}", a, c)
        } else if op == 1 {
            format!("\tlsl\tx{}, x{}, x{}", a, b, c)
        } else if op == 2 {
            format!("\tasr\tx{}, x{}, x{}", a, b, c)
        } else if op == 3 {
            format!("\tror\tx{}, x{}, x{}", a, b, c)
        } else if op == 4 {
            format!("\tand\tx{}, x{}, x{}", a, b, c)
        } else if op == 5 {
            format!("\tbic\tx{}, x{}, x{}", a, b, c)
        } else if op == 6 {
            format!("\torr\tx{}, x{}, x{}", a, b, c)
        } else if op == 7 {
            format!("\teor\tx{}, x{}, x{}", a, b, c)
        } else if op == 8 {
            format!("\tadd\tx{}, x{}, x{}", a, b, c)
        } else if op == 9 {
            format!("\tsub\tx{}, x{}, x{}", a, b, c)
        } else if op == 10 {
            format!("\tmul\tx{}, x{}, x{}", a, b, c)
        } else if op == 11 {
            format!("\tsdiv\tx{}, x{}, x{}", a, b, c)
        } else if op >= 12 && op <= 15 {
            let op_names = ["fadd", "fsub", "fmul", "fdiv"];
            format!("\t{}\td{}, d{}, d{}", op_names[(op - 12) as usize], a, b, c)
        } else {
            format!("\t// Unknown op {}", op)
        }
    } else if k == 1 {
        let imm = imm_signed.unwrap();
        if op == 0 {
            format!("\tmov\tx{}, #{}", a, imm)
        } else if op == 1 {
            format!("\tlsl\tx{}, x{}, #{}", a, b, imm & 0x1F)
        } else if op == 2 {
            format!("\tasr\tx{}, x{}, #{}", a, b, imm & 0x1F)
        } else if op == 3 {
            format!("\tror\tx{}, x{}, #{}", a, b, imm & 0x1F)
        } else if op == 4 {
            format!("\tand\tx{}, x{}, #{}", a, b, imm)
        } else if op == 5 {
            format!("\tbic\tx{}, x{}, #{}", a, b, imm)
        } else if op == 6 {
            format!("\torr\tx{}, x{}, #{}", a, b, imm)
        } else if op == 7 {
            format!("\teor\tx{}, x{}, #{}", a, b, imm)
        } else if op == 8 {
            format!("\tadd\tx{}, x{}, #{}", a, b, imm)
        } else if op == 9 {
            format!("\tsub\tx{}, x{}, #{}", a, b, imm)
        } else {
            format!("\t// TODO: op {} with immediate", op)
        }
    } else if k == 2 {
        let disp = disp_signed.unwrap();
        if u == 1 {
            format!("\tstr\tx{}, [x{}, #{}]", a, b, disp)
        } else {
            format!("\tldr\tx{}, [x{}, #{}]", a, b, disp)
        }
    } else if k == 3 {
        let link = (instr >> 28) & 0x1 != 0;
        if u == 0 {
            if link {
                format!("\tblr\tx{}", c)
            } else {
                format!("\tbr\tx{}", c)
            }
        } else {
            let disp = disp_signed.unwrap();
            let offset_words = disp as i64 + 1;
            let byte_offset = offset_words * 4;
            let dest = fix_target.map(String::from);
            match (cond, link, dest) {
                (BranchCondition::Always, true, Some(target)) => format!("\tbl\t{}", target),
                (BranchCondition::Always, false, Some(target)) => format!("\tb\t{}", target),
                (BranchCondition::Always, true, None) => format!("\tbl\t.+{}", byte_offset),
                (BranchCondition::Always, false, None) => format!("\tb\t.+{}", byte_offset),
                (BranchCondition::Known(name), _, Some(target)) => {
                    format!("\tb.{}\t{}", name.to_lowercase(), target)
                }
                (BranchCondition::Known(name), _, None) => {
                    format!("\tb.{}\t.+{}", name.to_lowercase(), byte_offset)
                }
                (BranchCondition::Unknown(_), true, Some(target)) => {
                    format!("\tbl\t{} // unknown cond", target)
                }
                (BranchCondition::Unknown(_), false, Some(target)) => {
                    format!("\tb\t{} // unknown cond", target)
                }
                (BranchCondition::Unknown(_), true, None) => {
                    format!("\tbl\t.+{} // unknown cond", byte_offset)
                }
                (BranchCondition::Unknown(_), false, None) => {
                    format!("\tb\t.+{} // unknown cond", byte_offset)
                }
            }
        }
    } else {
        format!("\t// Unknown instruction class k={}", k)
    };

    if annotation.is_empty() {
        format!("{}\t\t// {}", aarch64, risc5)
    } else {
        format!("{}\t\t// {}{}", aarch64, risc5, annotation)
    }
}

fn process_rsc(filename: &str, debug: bool) -> io::Result<()> {
    let mut file = File::open(filename)?;
    
    // Read module name
    file.seek(SeekFrom::Start(0))?;
    let mut name_bytes = Vec::new();
    let mut mod_name = None;
    
    loop {
        let mut buf = [0u8; 1];
        match file.read_exact(&mut buf) {
            Ok(_) => {
                if buf[0] == 0 {
                    mod_name = Some(String::from_utf8_lossy(&name_bytes).to_string());
                    break;
                }
                if buf[0] < b' ' || buf[0] > b'~' {
                    file.seek(SeekFrom::Start(0))?;
                    break;
                }
                name_bytes.push(buf[0]);
            }
            Err(_) => {
                eprintln!("Error: {} is empty or truncated", filename);
                return Ok(());
            }
        }
    }
    
    println!("# Generated from {}{}", filename, 
             mod_name.as_ref().map(|n| format!(" (module: {})", n)).unwrap_or_default());
    println!("\t.text");
    
    // Read header
    let _key = read_i32(&mut file)?;
    let mut cls_byte = [0u8; 1];
    file.read_exact(&mut cls_byte)?;
    let _cls = if cls_byte[0] < 0x80 { cls_byte[0] as i32 } else { cls_byte[0] as i32 - 0x100 };
    let _size = read_i32(&mut file)?;
    
    // Read imports
    let mut imports = Vec::new();
    loop {
        let name = read_cstring(&mut file)?;
        if name.is_empty() {
            break;
        }
        let _key = read_i32(&mut file)?;
        imports.push(name);
    }
    
    if !imports.is_empty() {
        println!("# Imports: {}", imports.join(", "));
    }
    
    // Type descriptors
    let n = read_i32(&mut file)?;
    let td_count = n / 4;
    for _ in 0..td_count {
        let _ = read_i32(&mut file)?;
    }
    
    // Data section
    let _ = read_i32(&mut file)?;
    
    // Strings section
    let nstr = read_i32(&mut file)?;
    let mut _str_buf = vec![0u8; nstr as usize];
    file.read_exact(&mut _str_buf)?;
    
    // Code section
    let code_n = read_i32(&mut file)?;
    let mut instructions = Vec::new();
    for _ in 0..code_n {
        instructions.push(read_u32(&mut file)?);
    }
    
    // Commands section
    loop {
        let name = read_cstring(&mut file)?;
        if name.is_empty() {
            break;
        }
        let _adr = read_i32(&mut file)?;
    }
    
    // Entries section
    let n = read_i32(&mut file)?;
    for _ in 0..n {
        let _ = read_i32(&mut file)?;
    }
    
    // Pointer refs
    loop {
        let data = read_i32(&mut file)?;
        if data == -1 {
            break;
        }
    }
    
    // Fixup information
    let fix_p = read_i32(&mut file)?;
    let fix_d = read_i32(&mut file)?;
    let fix_t = read_i32(&mut file)?;
    let entry = read_i32(&mut file)?;
    
    // Build fixup map
    // Build fixup map
    let mut fixup_map = HashMap::new();

    // --- fixP: procedure-call fixups (walk backwards by disp words; stop at word 0) ---
    if fix_p != 0 {
        let mut current = fix_p as usize;
        debug_println!(debug, "# FixP chain starting at word {} (byte {})", current, fix_p * 4);

        let mut seen = std::collections::HashSet::new();
        let mut count = 0usize;

        while current != 0 && current < instructions.len() {
            if !seen.insert(current) {
                debug_println!(debug, "# Loop detected at word {}", current);
                break;
            }
            let instr = instructions[current];
            let mno = (instr / 0x100000) % 0x10;
            let pno = (instr / 0x1000) % 0x100;
            let disp = (instr % 0x1000) as usize;

            debug_println!(
                debug,
                "# fixP node {}: word {}, instr={:08x}  mno={} pno={} disp={}",
                count + 1,
                current,
                instr,
                mno,
                pno,
                disp
            );

            // Build human-readable tag (imports are 1-based in mno)
            let annotation = if mno > 0 && (mno as usize) <= imports.len() {
                format!("{}.Proc{}", imports[(mno - 1) as usize], pno)
            } else if mno == 0 {
                format!("Self.Proc{}", pno)
            } else {
                format!("Mod{}.Item{}", mno, pno)
            };
            fixup_map.insert(current, annotation.clone());
            debug_println!(debug, "#   -> {}", annotation);

            if disp == 0 || current < disp {
                debug_println!(debug, "#   Stopping: invalid disp={} from word {}", disp, current);
                break;
            }
            current -= disp;
            count += 1;
            if count > 100000 {
                debug_println!(debug, "# Warning: fixP chain unreasonably long; stopping");
                break;
            }
        }
        debug_println!(debug, "# Found {} fixups in procedure chain\n", count);
    }

    // --- fixD: data-reference fixups (same chain walk; annotate as Var) ---
    if fix_d != 0 {
        let mut current = fix_d as usize;
        debug_println!(debug, "# FixD chain starting at word {} (byte {})", current, fix_d * 4);

        let mut seen = std::collections::HashSet::new();
        let mut count = 0usize;

        while current != 0 && current < instructions.len() {
            if !seen.insert(current) {
                debug_println!(debug, "# Loop detected at word {}", current);
                break;
            }
            let instr = instructions[current];
            let mno = (instr / 0x100000) % 0x10;
            let vno = (instr / 0x1000) % 0x100; // best-effort; exact vno may live in next word for some encodings
            let disp = (instr % 0x1000) as usize;

            debug_println!(
                debug,
                "# fixD node {}: word {}, instr={:08x}  mno={} vno~={} disp={}",
                count + 1,
                current,
                instr,
                mno,
                vno,
                disp
            );

            let annotation = if mno > 0 && (mno as usize) <= imports.len() {
                format!("{}.Var{}", imports[(mno - 1) as usize], vno)
            } else if mno == 0 {
                format!("Global.Var{}", vno)
            } else {
                format!("Mod{}.Var{}", mno, vno)
            };
            fixup_map.insert(current, annotation.clone());
            debug_println!(debug, "#   -> {}", annotation);

            if disp == 0 || current < disp {
                debug_println!(debug, "#   Stopping: invalid disp={} from word {}", disp, current);
                break;
            }
            current -= disp;
            count += 1;
            if count > 100000 {
                debug_println!(debug, "# Warning: fixD chain unreasonably long; stopping");
                break;
            }
        }
        debug_println!(debug, "# Found {} fixups in data chain\n", count);
    }

    // Print instructions
    println!("# Code words: {}", instructions.len());

    if entry >= 0 {
        let entry_word = (entry as usize) / 4;
        if entry_word < instructions.len() {
            let entry_word_i64 = entry_word as i64;
            let byte_offset = (entry_word_i64 + 1) * 4;
            println!("-1:\tb\t.+{}\t\t// entry -> line {}", byte_offset, entry_word);
        }
    }

    for (i, instr) in instructions.iter().enumerate() {
        println!("{:4}:\t{}", i, translate_instruction(*instr, &fixup_map, i));
    }
    if debug {
        println!("\n# Fixups:");
        println!("# fixP: {}", fix_p);
        println!("# fixD: {}", fix_d);
        println!("# fixT: {}", fix_t);
        println!("# entry: {}", entry);
    }
    
    Ok(())
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut debug = false;
    let mut filename: Option<String> = None;

    for arg in &args[1..] {
        if arg == "--debug" {
            debug = true;
        } else if filename.is_none() {
            filename = Some(arg.clone());
        } else {
            eprintln!("Usage: rsc2aarch64 [--debug] <file.rsc>");
            process::exit(1);
        }
    }

    let filename = match filename {
        Some(name) => name,
        None => {
            eprintln!("Usage: rsc2aarch64 [--debug] <file.rsc>");
            process::exit(1);
        }
    };

    if let Err(e) = process_rsc(&filename, debug) {
        eprintln!("Error processing file: {}", e);
        process::exit(1);
    }
}
