// #![cfg(target_arch = "wasm32")]

use wasm_bindgen::prelude::*;

use crate::{compiler::compile, eval::Vm};

/// # Errors
///
/// Returns any compilation errors, as a string
#[wasm_bindgen]
pub fn compile_str(string: &str) -> Result<String, String> {
    let mut vm = Vm::default();
    let prog = compile(string.bytes(), &mut vm);
    prog.map(|c| format!("{:?}", c.function().chunk())).map_err(|e| e.to_string())
}

/// # Errors
///
/// Returns any compilation or execution errors, as a string
#[wasm_bindgen]
pub fn run(string: &str) -> Result<Vec<String>, String> {
    let stdout = String::new(); // TODO capture

    let res = {
        let mut vm = Vm::new();
        let prog = compile(string.bytes(), &mut vm).map_err(|e| e.to_string())?;
        let res = vm.interpret(prog.into());
        res.map_err(|e| e.to_string())?.to_string()
    };

    Ok(vec![stdout, res])
}
