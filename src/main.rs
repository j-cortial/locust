mod chunk;
use chunk::{Chunk, OpCode};

mod debug;
use debug::disassemble;

use std::env;

fn main() -> Result<(), ()> {
    let _args: Vec<String> = env::args().collect();

    let mut chunk = Chunk::new();
    chunk.write(OpCode::OpReturn as u8);
    disassemble(&chunk, "test chunk");

    Ok(())
}
