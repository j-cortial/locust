use crate::{chunk::{self, OP_CONSTANT}, debug::disassemble_instruction};
use chunk::{Chunk, OP_RETURN};

use crate::value;
use value::print_value;

pub struct VM<'a> {
    chunk: &'a Chunk,
    ip: usize,
}

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

impl<'a> VM<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        Self { chunk, ip: 0 }
    }

    pub fn interpret(&mut self) -> InterpretResult {
        self.run()
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            #[cfg(feature = "debug_trace_execution")]
            {
                disassemble_instruction(self.chunk, self.ip);
            }
            let instruction = self.read_byte();
            match instruction {
                OP_CONSTANT => {
                    let constant = self.chunk.constants()[self.read_byte() as usize];
                    println!("{constant}");
                }
                OP_RETURN => return InterpretResult::Ok,
                _ => {},
            }
        }
    }

    fn read_byte(&mut self) -> u8 {
        let res = self.chunk[self.ip];
        self.ip += 1;
        res
    }
}
