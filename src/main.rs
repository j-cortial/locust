mod chunk;
mod debug;
mod value;
mod vm;

use vm::{InterpretResult, VM};

use std::{env, fs, io::Write, process::ExitCode};

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();

    let mut vm = VM::new();

    if args.len() == 1 {
        repl(&mut vm);
    } else if args.len() == 2 {
        match run_file(&mut vm, &args[1]) {
            Ok(_) => {}
            Err(error_code) => return error_code,
        }
    } else {
        eprintln!("Usage: locust [path]");
    }

    ExitCode::SUCCESS
}

fn repl(vm: &mut VM) {
    loop {
        let mut line = String::new();
        print!("> ");
        let _ = std::io::stdout().flush();
        std::io::stdin().read_line(&mut line).unwrap();
        if line.starts_with('\n') {
            break;
        }
        vm.interpret(&line);
    }
}

fn run_file(vm: &mut VM, path: &str) -> Result<(), ExitCode> {
    let source = read_file(path)?;
    match vm.interpret(&source) {
        InterpretResult::Ok => Ok(()),
        InterpretResult::CompileError => Err(ExitCode::from(65)),
        InterpretResult::RuntimeError => Err(ExitCode::from(70)),
    }
}

fn read_file(path: &str) -> Result<String, ExitCode> {
    match fs::read_to_string(path) {
        Ok(res) => Ok(res),
        Err(_) => {
            eprintln!("Could not open file \"{path}\"");
            Err(ExitCode::from(74))
        },
    }
}
