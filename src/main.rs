mod chunk;
mod value;
use chunk::{Chunk, OpCode, OP_CONSTANT};

mod debug;
use debug::disassemble;

use std::env;

fn main() -> Result<(), ()> {
    let _args: Vec<String> = env::args().collect();

    let mut chunk = Chunk::new();
    let constant = chunk.add_constant(1.2);
    chunk.write(OpCode::OpConstant as u8, 123);
    chunk.write(constant as u8, 123);
    chunk.write(OpCode::OpReturn as u8, 123);
    disassemble(&chunk, "test chunk");

    Ok(())
}
