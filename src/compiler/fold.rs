use std::rc::Rc;

use super::{
    bytecode::{BinArgs, Instruction, NVArgs, ParseError, VNArgs},
    Compiler, ExprDesc, ExprKind,
};

pub(super) trait BinFolder {
    fn is_foldable(&self, expr: &ExprDesc) -> bool;

    fn get_res<T: Iterator<Item = u8> + Clone>(
        self,
        compiler: &mut Compiler<'_, T>,
        lhs: ExprDesc,
        rhs: ExprDesc,
        line: usize,
    ) -> Result<ExprDesc, ParseError>;
}

#[allow(clippy::similar_names)]
pub struct CommutativeFolder<
    Folder: FnOnce(f64, f64) -> f64,
    I1: FnOnce(BinArgs) -> Instruction,
    I2: FnOnce(VNArgs) -> Instruction,
> {
    folder: Folder,
    vv_instr: I1,
    vn_instr: I2,
}

#[allow(clippy::similar_names)]
pub struct NonCommutativeFolder<
    Folder: FnOnce(f64, f64) -> f64,
    I1: FnOnce(BinArgs) -> Instruction,
    I2: FnOnce(VNArgs) -> Instruction,
    I3: FnOnce(NVArgs) -> Instruction,
> {
    folder: Folder,
    vv_instr: I1,
    vn_instr: I2,
    nv_instr: I3,
}

pub struct StringFolder<
    Folder: FnOnce(Rc<[u8]>, Rc<[u8]>) -> Rc<[u8]>,
    I1: FnOnce(BinArgs) -> Instruction,
> {
    folder: Folder,
    vv_instr: I1,
}

impl<
        Folder: FnOnce(f64, f64) -> f64,
        I1: FnOnce(BinArgs) -> Instruction,
        I2: FnOnce(VNArgs) -> Instruction,
    > CommutativeFolder<Folder, I1, I2>
{
    #[allow(clippy::similar_names)]
    pub(super) const fn new(folder: Folder, vv_instr: I1, vn_instr: I2) -> Self {
        Self { folder, vv_instr, vn_instr }
    }
}

impl<
        Folder: FnOnce(f64, f64) -> f64,
        I1: FnOnce(BinArgs) -> Instruction,
        I2: FnOnce(VNArgs) -> Instruction,
    > BinFolder for CommutativeFolder<Folder, I1, I2>
{
    fn is_foldable(&self, expr: &ExprDesc) -> bool {
        expr.as_number().is_some()
    }

    fn get_res<T: Iterator<Item = u8> + Clone>(
        self,
        compiler: &mut Compiler<'_, T>,
        mut lhs: ExprDesc,
        mut rhs: ExprDesc,
        line: usize,
    ) -> Result<ExprDesc, ParseError> {
        let dst = u8::MAX;
        let retval = match (lhs.as_number(), rhs.as_number()) {
            (Some(n1), Some(n2)) => {
                let folded = (self.folder)(n1, n2);
                Ok(ExprDesc::new(ExprKind::Number(folded)))
            }
            (Some(n), None) | (None, Some(n)) => {
                let handle = compiler.current_chunk_mut().new_number_constant(n)?.try_into();
                if let Ok(handle) = handle {
                    let v = if lhs.as_number().is_some() {
                        rhs.to_any_reg(compiler)?
                    } else {
                        lhs.to_any_reg(compiler)?
                    };
                    let instr_idx =
                        Some(compiler.instruction(
                            (self.vn_instr)(VNArgs { dst, lhs: v, rhs: handle }),
                            line,
                        ));

                    Ok(ExprDesc::new(ExprKind::Tmp { reg: dst, instr_idx }))
                } else {
                    // Constant handle is too big,
                    // discharge constant to reg and use VV operation
                    if lhs.as_number().is_some() {
                        lhs.to_any_reg(compiler)?;
                    } else {
                        rhs.to_any_reg(compiler)?;
                    };
                    return self.get_res(compiler, lhs, rhs, line);
                }
            }
            (None, None) => {
                let lhs = lhs.to_any_reg(compiler)?;
                let rhs = rhs.to_any_reg(compiler)?;
                let instr_idx =
                    Some(compiler.instruction((self.vv_instr)(BinArgs { dst, lhs, rhs }), line));

                Ok(ExprDesc::new(ExprKind::Tmp { reg: dst, instr_idx }))
            }
        };

        rhs.free_reg(compiler);
        lhs.free_reg(compiler);

        retval
    }
}

impl<
        Folder: FnOnce(f64, f64) -> f64,
        I1: FnOnce(BinArgs) -> Instruction,
        I2: FnOnce(VNArgs) -> Instruction,
        I3: FnOnce(NVArgs) -> Instruction,
    > NonCommutativeFolder<Folder, I1, I2, I3>
{
    #[allow(clippy::similar_names)]
    pub(super) const fn new(folder: Folder, vv_instr: I1, vn_instr: I2, nv_instr: I3) -> Self {
        Self { folder, vv_instr, vn_instr, nv_instr }
    }
}

impl<
        Folder: FnOnce(f64, f64) -> f64,
        I1: FnOnce(BinArgs) -> Instruction,
        I2: FnOnce(VNArgs) -> Instruction,
        I3: FnOnce(NVArgs) -> Instruction,
    > BinFolder for NonCommutativeFolder<Folder, I1, I2, I3>
{
    fn is_foldable(&self, expr: &ExprDesc) -> bool {
        expr.as_number().is_some()
    }

    fn get_res<T: Iterator<Item = u8> + Clone>(
        self,
        compiler: &mut Compiler<'_, T>,
        mut lhs: ExprDesc,
        mut rhs: ExprDesc,
        line: usize,
    ) -> Result<ExprDesc, ParseError> {
        let dst = u8::MAX;
        let retval = match (lhs.as_number(), rhs.as_number()) {
            (Some(n1), Some(n2)) => {
                let folded = (self.folder)(n1, n2);
                Ok(ExprDesc::new(ExprKind::Number(folded)))
            }
            (Some(n), None) => {
                let handle = compiler.current_chunk_mut().new_number_constant(n)?.try_into();
                if let Ok(handle) = handle {
                    let v = rhs.to_any_reg(compiler)?;
                    let instr_idx =
                        Some(compiler.instruction(
                            (self.nv_instr)(NVArgs { dst, lhs: handle, rhs: v }),
                            line,
                        ));

                    Ok(ExprDesc::new(ExprKind::Tmp { reg: dst, instr_idx }))
                } else {
                    // Constant handle is too big,
                    // discharge constant to reg and use VV operation
                    lhs.to_any_reg(compiler)?;
                    return self.get_res(compiler, lhs, rhs, line);
                }
            }
            (None, Some(n)) => {
                let handle = compiler.current_chunk_mut().new_number_constant(n)?.try_into();
                if let Ok(handle) = handle {
                    let v = lhs.to_any_reg(compiler)?;
                    let instr_idx =
                        Some(compiler.instruction(
                            (self.vn_instr)(VNArgs { dst, lhs: v, rhs: handle }),
                            line,
                        ));

                    Ok(ExprDesc::new(ExprKind::Tmp { reg: dst, instr_idx }))
                } else {
                    // Constant handle is too big,
                    // discharge constant to reg and use VV operation
                    rhs.to_any_reg(compiler)?;
                    return self.get_res(compiler, lhs, rhs, line);
                }
            }
            (_, _) => {
                let lhs = lhs.to_any_reg(compiler)?;
                let rhs = rhs.to_any_reg(compiler)?;
                let instr_idx =
                    Some(compiler.instruction((self.vv_instr)(BinArgs { dst, lhs, rhs }), line));

                Ok(ExprDesc::new(ExprKind::Tmp { reg: dst, instr_idx }))
            }
        };

        rhs.free_reg(compiler);
        lhs.free_reg(compiler);

        retval
    }
}

impl<Folder: FnOnce(Rc<[u8]>, Rc<[u8]>) -> Rc<[u8]>, I1: FnOnce(BinArgs) -> Instruction>
    StringFolder<Folder, I1>
{
    pub(super) const fn new(folder: Folder, vv_instr: I1) -> Self {
        Self { folder, vv_instr }
    }
}

impl<Folder: FnOnce(Rc<[u8]>, Rc<[u8]>) -> Rc<[u8]>, I1: FnOnce(BinArgs) -> Instruction> BinFolder
    for StringFolder<Folder, I1>
{
    fn is_foldable(&self, expr: &ExprDesc) -> bool {
        expr.as_str().is_some()
    }

    fn get_res<T: Iterator<Item = u8> + Clone>(
        self,
        compiler: &mut Compiler<'_, T>,
        mut lhs: ExprDesc,
        mut rhs: ExprDesc,
        line: usize,
    ) -> Result<ExprDesc, ParseError> {
        let dst = u8::MAX;
        let retval = if let (Some(s1), Some(s2)) = (lhs.as_str(), rhs.as_str()) {
            let folded = compiler.tokens.vm().new_string((self.folder)(s1.inner(), s2.inner()));
            Ok(ExprDesc::new(ExprKind::String(folded)))
        } else {
            let lhs = lhs.to_any_reg(compiler)?;
            let rhs = rhs.to_any_reg(compiler)?;
            let instr_idx =
                Some(compiler.instruction((self.vv_instr)(BinArgs { dst, lhs, rhs }), line));

            Ok(ExprDesc::new(ExprKind::Tmp { reg: dst, instr_idx }))
        };

        rhs.free_reg(compiler);
        lhs.free_reg(compiler);
        retval
    }
}
