mod chunk;
use chunk::{Chunk, OpCode};

mod value;

mod debug;
use debug::disassemble;

mod vm;
use vm::VM;

use std::env;

fn main() -> Result<(), ()> {
    let _args: Vec<String> = env::args().collect();

    let mut chunk = Chunk::new();
    let constant = chunk.add_constant(1.2);
    chunk.write(OpCode::OpConstant as u8, 123);
    chunk.write(constant as u8, 123);
    chunk.write(OpCode::OpReturn as u8, 123);
    let mut vm = VM::new(&chunk);
    vm.interpret();
    disassemble(&chunk, "test chunk");

    Ok(())
}
