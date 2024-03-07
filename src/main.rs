mod chunk;
use chunk::{Chunk, OP_ADD, OP_CONSTANT, OP_DIVIDE, OP_NEGATE, OP_RETURN};

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
    chunk.write(OP_CONSTANT, 123);
    chunk.write(constant as u8, 123);

    let constant = chunk.add_constant(3.4);
    chunk.write(OP_CONSTANT, 123);
    chunk.write(constant as u8, 123);

    chunk.write(OP_ADD, 123);

    let constant = chunk.add_constant(5.6);
    chunk.write(OP_CONSTANT, 123);
    chunk.write(constant as u8, 123);

    chunk.write(OP_DIVIDE, 123);
    chunk.write(OP_NEGATE, 123);

    chunk.write(OP_RETURN, 123);

    let mut vm = VM::new(&chunk);
    vm.interpret();
    disassemble(&chunk, "test chunk");

    Ok(())
}
