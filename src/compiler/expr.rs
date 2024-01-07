use crate::eval::vals::{function::Function, string::RuaString};

use super::{
    bytecode::{BinArgs, Instruction as I, ParseError, UnArgs},
    upvalues::Upvalues,
    Compiler,
};

#[derive(Debug)]
pub(super) enum ExprKind {
    Local { reg: u8 }, // instr_idx = None means expr is non relocable
    Global(RuaString),
    Upvalue(RuaString),
    Tmp { reg: u8, instr_idx: Option<usize> }, // instr_idx = None means expr is non relocable
    Number(f64),
    String(RuaString),
    Function(Box<(Function, Upvalues)>),
    Index { table: u8, key: u8 },
    True,
    False,
    Nil,
    Jmp { instr_idx: usize },
}

#[derive(Debug)]
pub(super) struct ExprDesc {
    pub(super) kind: ExprKind,
    pub(super) t_jmp: Option<usize>,
    pub(super) f_jmp: Option<usize>,
}

#[allow(clippy::wrong_self_convention)]
impl ExprDesc {
    pub(super) const fn new(kind: ExprKind) -> Self {
        Self { kind, t_jmp: None, f_jmp: None }
    }

    pub(super) fn to_any_reg<T: Iterator<Item = u8> + Clone>(
        &mut self,
        compiler: &mut Compiler<'_, T>,
    ) -> Result<u8, ParseError> {
        match &self.kind {
            ExprKind::Tmp { reg, .. } | ExprKind::Local { reg, .. } if *reg != 255 => {
                let reg = *reg;
                if !self.has_jmps() {
                    return Ok(reg);
                } else if compiler.is_tmp(reg) {
                    self.to_reg(compiler, reg)?;
                    return Ok(reg);
                }
            }
            _ => {}
        };
        self.to_next_reg(compiler)
    }

    pub(super) fn free_reg<T: Iterator<Item = u8> + Clone>(&self, compiler: &mut Compiler<'_, T>) {
        if let ExprKind::Tmp { reg, .. } = &self.kind {
            if *reg != u8::MAX {
                compiler.free_reg(*reg);
            }
        }
    }

    pub(super) fn skip_if_false<T: Iterator<Item = u8> + Clone>(
        &mut self,
        compiler: &mut Compiler<'_, T>,
        line: usize,
        store_val: bool,
    ) -> Result<Option<usize>, ParseError> {
        let jmp = match &self.kind {
            ExprKind::Jmp { instr_idx } => Some(*instr_idx),
            ExprKind::Number(_) | ExprKind::String(_) | ExprKind::Function(_) | ExprKind::True => {
                None
            }
            _ => {
                let src = self.to_any_reg(compiler)?;
                compiler.free_reg(src);
                if store_val {
                    let jmp_idx = compiler.cond_jmp(I::TestSet { dst: u8::MAX, src }, line);
                    self.kind = ExprKind::Tmp { reg: u8::MAX, instr_idx: Some(jmp_idx - 1) };
                    Some(jmp_idx)
                } else {
                    let jmp_idx = compiler.cond_jmp(I::Test { src }, line);
                    Some(jmp_idx)
                }
            }
        };
        compiler.concat_jmp_lists(&mut self.f_jmp, jmp)?;
        compiler.patch_list_to_here(self.t_jmp.take())?;
        Ok(jmp)
    }

    pub(super) fn skip_if_true<T: Iterator<Item = u8> + Clone>(
        &mut self,
        compiler: &mut Compiler<'_, T>,
        line: usize,
    ) -> Result<Option<usize>, ParseError> {
        let jmp = match &self.kind {
            ExprKind::Jmp { instr_idx } => {
                compiler.negate_cond(*instr_idx - 1);
                Some(*instr_idx)
            }
            ExprKind::False | ExprKind::Nil => None,
            _ => {
                let src = self.to_any_reg(compiler)?;
                compiler.free_reg(src);
                let jmp_idx = compiler.cond_jmp(I::UntestSet { dst: u8::MAX, src }, line);
                self.kind = ExprKind::Tmp { reg: u8::MAX, instr_idx: Some(jmp_idx - 1) };
                Some(jmp_idx)
            }
        };
        compiler.concat_jmp_lists(&mut self.t_jmp, jmp)?;
        compiler.patch_list_to_here(self.f_jmp.take())?;
        Ok(jmp)
    }

    const fn has_jmps(&self) -> bool {
        !matches!((self.t_jmp, self.f_jmp), (None, None))
    }

    pub(super) fn to_reg<T: Iterator<Item = u8> + Clone>(
        &mut self,
        compiler: &mut Compiler<'_, T>,
        dst: u8,
    ) -> Result<(), ParseError> {
        match &self.kind {
            ExprKind::Local { reg } => {
                if *reg != dst {
                    compiler.instruction(I::Mv(UnArgs { dst, src: *reg }), 0);
                }
            }
            ExprKind::Tmp { reg, instr_idx } => {
                if *reg != dst {
                    if let Some(instr_idx) = instr_idx {
                        compiler.current_chunk_mut().chg_dst_of(*instr_idx, dst);
                    } else {
                        compiler.instruction(I::Mv(UnArgs { dst, src: *reg }), 0);
                    }
                }
            }
            ExprKind::Global(s) => {
                let src = compiler.new_string_constant(s.clone())?;
                compiler.instruction(I::GetGlobal { dst, src }, 0);
            }
            ExprKind::Upvalue(up) => {
                let src = compiler
                    .context
                    .resolve_upvalue(up, &mut compiler.context_stack)?
                    .expect("Got invalid upvalue");
                compiler.instruction(I::GetUpvalue { dst, src }, 0);
            }
            ExprKind::Number(n) => {
                compiler.emit_number(dst, *n, 0)?;
            }
            ExprKind::String(s) => {
                compiler.emit_string(dst, s.clone(), 0)?;
            }
            ExprKind::Function(_) => {
                let old_kind =
                    std::mem::replace(&mut self.kind, ExprKind::Tmp { reg: dst, instr_idx: None });
                if let ExprKind::Function(f) = old_kind {
                    let (func, upvals) = *f;
                    compiler.emit_closure(dst, func, upvals, 0)?;
                } else {
                    unreachable!("ExprKind was function")
                }
            }
            ExprKind::True => {
                compiler.instruction(I::True { dst }, 0);
            }
            ExprKind::False => {
                compiler.instruction(I::False { dst }, 0);
            }
            ExprKind::Nil => {
                compiler.instruction(I::Nil { dst }, 0);
            }
            ExprKind::Jmp { instr_idx } => {
                compiler.negate_cond(*instr_idx - 1);
                compiler.concat_jmp_lists(&mut self.t_jmp, Some(*instr_idx))?;
            }
            ExprKind::Index { table, key } => {
                compiler.instruction(I::Index(BinArgs { dst, lhs: *table, rhs: *key }), 0);
            }
        };
        if self.has_jmps() {
            let (mut pos_f, mut pos_t) = (None, None);
            if compiler.list_needs_val(self.t_jmp) || compiler.list_needs_val(self.f_jmp) {
                let fj = if let ExprKind::Jmp { .. } = self.kind {
                    None
                } else {
                    Some(compiler.instruction(I::Jmp(i16::MAX), 0))
                };
                pos_f = Some(compiler.instruction(I::LFalseSkip { dst }, 0));
                pos_t = Some(compiler.instruction(I::True { dst }, 0));
                if let Some(list) = fj {
                    compiler.patch_jmp_here(list, 0)?;
                }
            }
            let end = compiler.pc();
            compiler.patch_list_aux(self.f_jmp, end, dst, pos_f)?;
            compiler.patch_list_aux(self.t_jmp, end, dst, pos_t)?;
        }
        (self.f_jmp, self.t_jmp) = (None, None);
        self.kind = ExprKind::Tmp { reg: dst, instr_idx: None };
        Ok(())
    }

    pub(super) fn free_tmp_regs<T: Iterator<Item = u8> + Clone>(
        &self,
        compiler: &mut Compiler<'_, T>,
    ) {
        if let ExprKind::Index { table, key } = &self.kind {
            compiler.free2reg(*table, *key);
        }
    }

    pub(super) fn to_next_reg<T: Iterator<Item = u8> + Clone>(
        &mut self,
        compiler: &mut Compiler<'_, T>,
    ) -> Result<u8, ParseError> {
        self.free_tmp_regs(compiler);
        let tmp = compiler.tmp()?;
        self.to_reg(compiler, tmp)?;
        Ok(tmp)
    }

    pub(super) const fn as_number(&self) -> Option<f64> {
        if let ExprKind::Number(n) = self.kind {
            Some(n)
        } else {
            None
        }
    }
    pub(super) const fn as_str(&self) -> Option<&RuaString> {
        if let ExprKind::String(ref s) = self.kind {
            Some(s)
        } else {
            None
        }
    }
}
