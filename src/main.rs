mod parser;

use anyhow::Result;

fn main() -> Result<()> {
    parser::main();

    wasm_example()?;

    Ok(())
}

fn wasm_example() -> Result<()> {
    use wasmtime::{Engine, Instance, Module, Store};

    let binary = compile();

    // An engine stores and configures global compilation settings like
    // optimization level, enabled wasm features, etc.
    let engine = Engine::default();

    // Define the WASI functions globally on the `Config`.
    //let mut linker = Linker::new(&engine);
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

    /*linker.module(&mut store, "", &module)?;
    linker
        .get_default(&mut store, "")?
        .typed::<(), (), &Store<WasiCtx>>(&store)?
        .call(&mut store, ())?;*/

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
    let answer = answer.typed::<(i32, i32), i32>(&store)?;

    // And finally we can call our function! Note that the error propagation
    // with `?` is done to handle the case where the wasm function traps.
    let result = answer.call(&mut store, (1, 2))?;
    println!("Answer: {:?}", result);

    Ok(())
}

fn compile() -> Vec<u8> {
    use wasm_encoder::{
        CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction, Module,
        TypeSection, ValType,
    };

    // Encode the type section.
    let mut types = TypeSection::new();
    let params = vec![ValType::I32, ValType::I32];
    let results = vec![ValType::I32];
    types.function(params, results);

    // Encode the function section.
    let mut functions = FunctionSection::new();
    let type_index = 0;
    functions.function(type_index);

    // Encode the export section.
    let mut exports = ExportSection::new();
    exports.export("answer", ExportKind::Func, 0);

    // Encode the code section.
    let mut codes = CodeSection::new();
    let locals = vec![];
    let mut f = Function::new(locals);
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::End);
    codes.function(&f);

    let mut module = Module::new();
    module
        .section(&types)
        .section(&functions)
        .section(&exports)
        .section(&codes);

    // Extract the encoded Wasm bytes for this module.
    let wasm_bytes = module.finish();

    wasm_bytes
}
