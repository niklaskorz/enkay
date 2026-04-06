mod parser;

use anyhow::{Result, anyhow, bail};
use wasm_encoder::{BlockType, Function, InstructionSink, ValType};

use parser::ast;

fn main() -> Result<()> {
    let src = std::fs::read_to_string(std::env::args().nth(1).expect("Expected file argument"))
        .expect("Failed to read file");
    let ast = parser::parse(&src).unwrap();
    println!("{:#?}", ast);
    let binary = compile(ast)?;
    //for (offset, op) in binary.iter().enumerate() {
    //    println!("{:03} {:02X}", offset, op);
    //}
    std::fs::write("out.wasm", &binary)?;
    execute_wasm(binary)?;

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
    // of our input wasm module. In this case it'll be JIT-compiled.
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
        .typed::<(), i32>(&store)?
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
    // is to statically assert its signature with `typed` and then call it.
    let answer = answer.typed::<(i32, i32), i32>(&store)?;

    // And finally we can call our function! Note that the error propagation
    // with `?` is done to handle the case where the wasm function traps.
    let result = answer.call(&mut store, (1, 2))?;
    println!("Answer: {:?}", result);

    Ok(())
}

fn compile(ast: Vec<ast::Statement>) -> Result<Vec<u8>> {
    use wasm_encoder::{
        CodeSection, ExportKind, ExportSection, FunctionSection, Module, TypeSection,
    };

    let mut types = TypeSection::new();
    let mut functions = FunctionSection::new();
    let mut exports = ExportSection::new();
    let mut codes = CodeSection::new();

    let mut type_index = 0;
    for statement in ast {
        match statement {
            ast::Statement::FunctionDeclaration {
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
                types.ty().function(param_types, results);
                functions.function(type_index);
                exports.export(&name, ExportKind::Func, type_index);

                let declarations: u32 = body.iter().map(collect_declarations).sum();
                let locals = vec![(declarations, ValType::I32)];
                let mut function = Function::new(locals);
                let mut sink = function.instructions();
                let mut locals: Vec<_> = params.into_iter().map(|p| p.name).collect();
                for statement in body {
                    compile_statement(&mut sink, &mut locals, &statement)?;
                }
                sink.end();
                codes.function(&function);

                type_index += 1;
            }
            stmt => {
                bail!("unexpected top-level statement {:?}", stmt)
            }
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

// TODO: hacky placeholder while we only support i32 declarations
// This will be replaced with semantic analysis like in the previous (interpreted) version of my language:
// https://github.com/niklaskorz/nklang/tree/master/semantics
fn collect_declarations(statement: &ast::Statement) -> u32 {
    match statement {
        ast::Statement::Declaration(_, _) => 1,
        ast::Statement::Block(body) => body.iter().map(collect_declarations).sum(),
        ast::Statement::If(_, body, els) => body
            .iter()
            .map(collect_declarations)
            .chain(els.iter().map(|body| collect_declarations(body)))
            .sum(),
        ast::Statement::While(_, body) => body.iter().map(collect_declarations).sum(),
        _ => 0,
    }
}

fn compile_statement(
    sink: &mut InstructionSink,
    locals: &mut Vec<String>,
    statement: &ast::Statement,
) -> Result<()> {
    match statement {
        ast::Statement::Block(body) => {
            for statement in body {
                compile_statement(sink, locals, statement)?;
            }
        }
        ast::Statement::If(cond, body, els) => {
            compile_expression(sink, locals, cond)?;
            sink.if_(BlockType::Empty);
            for statement in body {
                compile_statement(sink, locals, statement)?;
            }
            if let Some(els) = els {
                sink.else_();
                compile_statement(sink, locals, els)?;
            }
            sink.end();
        }
        ast::Statement::While(cond, body) => {
            // break target
            sink.block(BlockType::Empty);
            // continue target
            sink.loop_(BlockType::Empty);

            // If the condition is false, we abort the loop by branching
            // the block around it.
            compile_expression(sink, locals, cond)?;
            // negate the condition
            sink.i32_const(1);
            sink.i32_xor();
            sink.br_if(1);

            for statement in body {
                compile_statement(sink, locals, statement)?;
            }

            // We branch unconditionally here as the loop condition is
            // checked at the beginning.
            // This way, we ensure the `continue` statement also goes through
            // the condition.
            sink.br(0);

            sink.end(); // loop
            sink.end(); // block
        }
        ast::Statement::Declaration(name, expr) => {
            compile_expression(sink, locals, expr)?;
            // We allow variable shadowing, so we don't check for naming conflicts
            let index = locals.len();
            locals.push(name.clone());
            sink.local_set(index.try_into().unwrap());
        }
        ast::Statement::Assignment(name, expr) => {
            compile_expression(sink, locals, expr)?;
            // We allow variable shadowing, so lookup from the back
            let index = locals
                .iter()
                .rposition(|l| l == name)
                .expect(&format!("unknown variable {}", name));
            sink.local_set(index.try_into().unwrap());
        }
        ast::Statement::Return(expr) => {
            if let Some(expr) = expr {
                compile_expression(sink, &locals, expr)?;
            }
            sink.return_();
        }
        ast::Statement::Continue => {
            // Continuing a loop in WASM works through branch instructions.
            // The branch instruction's behavior depends on the target.
            // Branching a block continues execution after the block,
            // while branching a loop goes back to the start of the loop.
            // The br parameter defines how many levels to go upwards, where
            // each loop/block along the way is one level.
            // TODO: needs the distance to the nearest loop
            let loop_dist = 0;
            sink.br(loop_dist);
        }
        ast::Statement::Break => {
            // TODO: needs the distance the nearest block
            let block_dist = 1;
            sink.br(block_dist);
        }
        ast::Statement::Expr(expr) => {
            compile_expression(sink, locals, expr)?;
            // TODO: only drop if the expression type isn't void
            // sink.drop();
        }
        stmt => {
            bail!("unexpected block-level statement {:?}", stmt);
        }
    }

    Ok(())
}

fn compile_expression(
    sink: &mut InstructionSink,
    locals: &Vec<String>,
    expr: &ast::Expr,
) -> Result<()> {
    match expr {
        ast::Expr::Ident(name) => {
            // We allow variable shadowing, so lookup from the back
            let index = locals
                .iter()
                .rposition(|l| l == name)
                .expect(&format!("unknown variable {}", name));
            sink.local_get(index.try_into().unwrap());
        }
        &ast::Expr::Decimal(val) => {
            sink.f64_const(val.into());
        }
        &ast::Expr::Integer(val) => {
            sink.i32_const(val.try_into().unwrap());
        }
        &ast::Expr::Boolean(val) => {
            // Boolean operations in WASM expect an i32 argument
            sink.i32_const(val.into());
        }
        ast::Expr::String(val) => {
            // TODO: no string repr yet
            sink.i32_const(val.len().try_into().unwrap());
        }
        ast::Expr::Array(val) => {
            // TODO: no array repr yet
            sink.i32_const(val.len().try_into().unwrap());
        }
        ast::Expr::Function { params, .. } => {
            // TODO: no anonymous functions yet
            sink.i32_const(params.len().try_into().unwrap());
        }
        ast::Expr::BinaryOp(lhs, op, rhs) => {
            compile_expression(sink, locals, lhs)?;
            compile_expression(sink, locals, rhs)?;
            match op {
                ast::BinaryOp::Multiply => {
                    sink.i32_mul();
                }
                ast::BinaryOp::Divide => {
                    sink.i32_div_s();
                }
                ast::BinaryOp::Plus => {
                    sink.i32_add();
                }
                ast::BinaryOp::Minus => {
                    sink.i32_sub();
                }
                ast::BinaryOp::Equal => {
                    sink.i32_eq();
                }
                ast::BinaryOp::NotEqual => {
                    sink.i32_ne();
                }
                ast::BinaryOp::GreaterOrEqual => {
                    sink.i32_ge_s();
                }
                ast::BinaryOp::LessOrEqual => {
                    sink.i32_le_s();
                }
                ast::BinaryOp::Greater => {
                    sink.i32_gt_s();
                }
                ast::BinaryOp::Less => {
                    sink.i32_lt_s();
                }
                ast::BinaryOp::LogicalAnd => {
                    sink.i32_and();
                }
                ast::BinaryOp::LogicalOr => {
                    sink.i32_or();
                }
            }
        }
        ast::Expr::PrefixOp(op, rhs) => match op {
            ast::PrefixOp::Minus => {
                sink.i32_const(0);
                compile_expression(sink, locals, rhs)?;
                sink.i32_sub();
            }
            ast::PrefixOp::Plus => {
                compile_expression(sink, locals, rhs)?;
            }
            op => {
                bail!("unexpected prefix op {:?}", op);
            }
        },
        expr => {
            bail!("unexpected expression {:?}", expr);
        }
    }

    Ok(())
}

fn map_type(type_name: &str) -> Result<wasm_encoder::ValType> {
    use wasm_encoder::ValType;
    match type_name {
        "i64" => Ok(ValType::I64),
        "i32" => Ok(ValType::I32),
        _ => Err(anyhow!("unknown type {}", type_name)),
    }
}
