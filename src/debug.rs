use crate::chunk;
use chunk::*;

pub fn disassemble(chunk: &Chunk, name: &str) {
    println!("== {name} ==");
    let mut offset = 0;
    while offset < chunk.count() {
        offset = disassemble_instruction(chunk, offset);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{offset:04} ");
    if offset > 0 && chunk.lines()[offset] == chunk.lines()[offset] {
        print!("   | ");
    } else {
        let line = chunk.lines()[offset];
        print!("{line:4} ");
    }
    let instruction = chunk[offset];
    match instruction {
        OP_CONSTANT => constant_instruction("OP_CONSTANT", chunk, offset),
        OP_ADD => simple_instruction("OP_ADD", offset),
        OP_SUBTRACT => simple_instruction("OP_SUBTRACT", offset),
        OP_MULTIPLY => simple_instruction("OP_MULTIPLY", offset),
        OP_DIVIDE => simple_instruction("OP_DIVIDE", offset),
        OP_NEGATE => simple_instruction("OP_NEGATE", offset),
        OP_RETURN => simple_instruction("OP_RETURN", offset),
        _ => {
            println!("Unknown opcode {instruction}");
            offset + 1
        }
    }
}

fn constant_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let constant = chunk[offset];
    let value = chunk.constants()[constant as usize];
    println!("{name:16} {constant:4} '{value}'");
    offset + 2
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{name}");
    offset + 1
}
