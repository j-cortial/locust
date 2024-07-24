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
    if offset > 0 && chunk.lines()[offset - 1] == chunk.lines()[offset] {
        print!("   | ");
    } else {
        let line = chunk.lines()[offset];
        print!("{line:4} ");
    }
    let instruction = chunk[offset];
    match instruction {
        OP_CONSTANT => constant_instruction("OP_CONSTANT", chunk, offset),
        OP_NIL => simple_instruction("OP_NIL", offset),
        OP_TRUE => simple_instruction("OP_TRUE", offset),
        OP_FALSE => simple_instruction("OP_FALSE", offset),
        OP_POP => simple_instruction("OP_POP", offset),
        OP_GET_LOCAL => byte_instruction("OP_GET_LOCAL", chunk, offset),
        OP_SET_LOCAL => byte_instruction("OP_SET_LOCAL", chunk, offset),
        OP_GET_GLOBAL => constant_instruction("OP_GET_GLOBAL", chunk, offset),
        OP_DEFINE_GLOBAL => constant_instruction("OP_DEFINE_GLOBAL", chunk, offset),
        OP_SET_GLOBAL => constant_instruction("OP_SET_GLOBAL", chunk, offset),
        OP_EQUAL => simple_instruction("OP_EQUAL", offset),
        OP_GREATER => simple_instruction("OP_GREATER", offset),
        OP_LESS => simple_instruction("OP_LESS", offset),
        OP_ADD => simple_instruction("OP_ADD", offset),
        OP_SUBTRACT => simple_instruction("OP_SUBTRACT", offset),
        OP_MULTIPLY => simple_instruction("OP_MULTIPLY", offset),
        OP_DIVIDE => simple_instruction("OP_DIVIDE", offset),
        OP_NOT => simple_instruction("OP_NOT", offset),
        OP_NEGATE => simple_instruction("OP_NEGATE", offset),
        OP_PRINT => simple_instruction("OP_PRINT", offset),
        OP_JUMP => jump_instruction("OP_JUMP", 1, chunk, offset),
        OP_JUMP_IF_FALSE => jump_instruction("OP_JUMP_IF_FALSE", 1, chunk, offset),
        OP_LOOP => jump_instruction("OP_LOOP", -1, chunk, offset),
        OP_RETURN => simple_instruction("OP_RETURN", offset),
        _ => {
            println!("Unknown opcode {instruction}");
            offset + 1
        }
    }
}

fn constant_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let constant = chunk[offset + 1];
    let value = chunk.constants()[constant as usize].clone();
    println!("{name:16} {constant:4} '{value}'");
    offset + 2
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{name}");
    offset + 1
}

fn byte_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let slot = chunk[offset + 1];
    println!("{name:16} {slot:4}");
    offset + 2
}

fn jump_instruction(name: &str, sign: i32, chunk: &Chunk, offset: usize) -> usize {
    let mut jump = (chunk[offset + 1] as u16) << 8;
    jump |= chunk[offset + 2] as u16;
    let to = if sign < 0 { (offset + 3) - jump as usize} else { offset + 3 + jump as usize };
    println!("{name:16} {offset:4} -> {to}");
    offset + 3
}
