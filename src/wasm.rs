// #![cfg(target_arch = "wasm32")]

use std::{cell::RefCell, io::Write, rc::Rc};

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
    let stdout = StdoutCapture::default();

    let res = {
        let mut vm = Vm::new(Box::new(stdout.clone()));
        let prog = compile(string.bytes(), &mut vm)
            .map_err(|e| "Compilation error".to_string() + &e.to_string())?;
        let res = vm.interpret(prog.into());
        res.map_err(|e| stdout.get() + "\n\nError thrown: " + &e.to_string())?.to_string()
    };

    Ok(vec![stdout.get(), res])
}

#[derive(Clone)]
struct StdoutCapture {
    inner: Rc<RefCell<Vec<u8>>>,
}

impl StdoutCapture {
    fn get(&self) -> String {
        let inner = self.inner.borrow();
        String::from_utf8_lossy(&inner).to_string()
    }
}

impl Default for StdoutCapture {
    fn default() -> Self {
        Self { inner: Rc::new(RefCell::new(Vec::default())) }
    }
}

impl Write for StdoutCapture {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let mut inner = self.inner.borrow_mut();
        inner.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        let mut inner = self.inner.borrow_mut();
        inner.flush()
    }
}
