mod parser;

use anyhow::{anyhow, Result};
use parser::Expr;
use wasm_encoder::Function;

use crate::parser::{Statement, Token};

fn main() -> Result<()> {
    let src = std::fs::read_to_string(std::env::args().nth(1).expect("Expected file argument"))
        .expect("Failed to read file");
    let ast = parser::parse(&src);
    if let Some(ast) = ast {
        let binary = compile(ast)?;
        execute_wasm(binary)?;
    }

    Ok(())
}

fn execute_wasm(binary: Vec<u8>) -> Result<()> {
    use wasmtime::{Engine, Instance, Linker, Module, Store};

    // An engine stores and configures global compilation settings like
    // optimization level, enabled wasm features, etc.
    let engine = Engine::default();

    // Define the WASI functions globally on the `Config`.
    let mut linker = Linker::new(&engine);
    //wasmtime_wasi::add_to_linker(&mut linker, |s| s)?;

    // We start off by creating a `Module` which represents a compiled form
    // of our input wasm module. In this case it'll be JIT-compiled after
    // we parse the text format.
    let module = Module::from_binary(&engine, &binary)?;

    // Create a WASI context and put it in a Store; all instances in the store
    // share this context. `WasiCtxBuilder` provides a number of ways to
    // configure what the target program will have access to.
    /*let wasi = WasiCtxBuilder::new()
    .inherit_stdio()
    .inherit_args()?
    .build();*/

    // A `Store` is what will own instances, functions, globals, etc. All wasm
    // items are stored within a `Store`, and it's what we'll always be using to
    // interact with the wasm world. Custom data can be stored in stores but for
    // now we just use `()`.
    let mut store = Store::new(&engine, ());

    linker.module(&mut store, "", &module)?;
    let result = linker
        .get_default(&mut store, "")?
        .typed::<(), i64>(&store)?
        .call(&mut store, ())?;
    println!("Result: {:?}", result);

    // With a compiled `Module` we can then instantiate it, creating
    // an `Instance` which we can actually poke at functions on.
    let instance = Instance::new(&mut store, &module, &[])?;

    // The `Instance` gives us access to various exported functions and items,
    // which we access here to pull out our `answer` exported function and
    // run it.
    let answer = instance
        .get_func(&mut store, "answer")
        .expect("`answer` was not an exported function");

    // There's a few ways we can call the `answer` `Func` value. The easiest
    // is to statically assert its signature with `typed` (in this case
    // asserting it takes no arguments and returns one i32) and then call it.
    let answer = answer.typed::<(i64, i64), i64>(&store)?;

    // And finally we can call our function! Note that the error propagation
    // with `?` is done to handle the case where the wasm function traps.
    let result = answer.call(&mut store, (1, 2))?;
    println!("Answer: {:?}", result);

    Ok(())
}

fn compile(ast: Vec<parser::Statement>) -> Result<Vec<u8>> {
    use wasm_encoder::{
        CodeSection, ExportKind, ExportSection, FunctionSection, Instruction, Module, TypeSection,
    };

    let mut types = TypeSection::new();
    let mut functions = FunctionSection::new();
    let mut exports = ExportSection::new();
    let mut codes = CodeSection::new();

    let mut type_index = 0;
    for statement in ast {
        match statement {
            Statement::FunctionDeclaration {
                name,
                params,
                return_type,
                body,
            } => {
                let param_types: Vec<_> = params
                    .iter()
                    .map(|p| map_type(&p.data_type).unwrap())
                    .collect();
                let results = vec![map_type(&return_type)?];
                types.function(param_types, results);
                functions.function(type_index);
                exports.export(&name, ExportKind::Func, type_index);

                let locals = vec![];
                let mut f = Function::new(locals);
                let locals: Vec<_> = params.into_iter().map(|p| p.name).collect();
                for statement in body {
                    match statement {
                        Statement::Return(expr) => {
                            if let Some(expr) = expr {
                                compile_expression(&mut f, &locals, &expr);
                            }
                            f.instruction(&Instruction::Return);
                        }
                        _ => {}
                    }
                }
                f.instruction(&Instruction::End);
                codes.function(&f);

                type_index += 1;
            }
            _ => {}
        }
    }

    let mut module = Module::new();
    module
        .section(&types)
        .section(&functions)
        .section(&exports)
        .section(&codes);

    Ok(module.finish())
}

fn compile_expression(f: &mut Function, locals: &Vec<String>, expr: &Expr) {
    use wasm_encoder::Instruction;
    match expr {
        Expr::Ident(name) => {
            f.instruction(&Instruction::LocalGet(
                locals
                    .iter()
                    .position(|l| l == name)
                    .unwrap()
                    .try_into()
                    .unwrap(),
            ));
        }
        &Expr::Decimal(val) => {
            f.instruction(&Instruction::F64Const(val));
        }
        &Expr::Integer(val) => {
            f.instruction(&Instruction::I64Const(val));
        }
        Expr::BinaryOp(lhs, op, rhs) => {
            compile_expression(f, locals, lhs);
            compile_expression(f, locals, rhs);
            match op {
                Token::Multiply => {
                    f.instruction(&Instruction::I64Mul);
                }
                Token::Divide => {
                    f.instruction(&Instruction::I64DivS);
                }
                Token::Plus => {
                    f.instruction(&Instruction::I64Add);
                }
                Token::Minus => {
                    f.instruction(&Instruction::I64Sub);
                }
                _ => {
                    f.instruction(&Instruction::Unreachable);
                }
            }
        }
        Expr::PrefixOp(op, rhs) => match op {
            Token::Minus => {
                f.instruction(&Instruction::I64Const(0));
                compile_expression(f, locals, rhs);
                f.instruction(&Instruction::I64Sub);
            }
            Token::Plus => {
                compile_expression(f, locals, rhs);
            }
            _ => {
                f.instruction(&Instruction::Unreachable);
            }
        },
        _ => {
            f.instruction(&Instruction::Unreachable);
        }
    }
}

fn map_type(type_name: &str) -> Result<wasm_encoder::ValType> {
    use wasm_encoder::ValType;
    match type_name {
        "i64" => Ok(ValType::I64),
        "i32" => Ok(ValType::I32),
        _ => Err(anyhow!("unknown type {}", type_name)),
    }
}
