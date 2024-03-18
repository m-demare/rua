use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

use crate::eval::{call_frame::CallFrame, Vm};

use super::{Callable, IntoRuaVal, RuaVal, RuaValInner};

#[derive(Debug, Clone, Copy)]
pub enum CoroutineStatus {
    Running,
    Normal,
    Suspended,
    Dead,
}

#[derive(Debug)]
enum CoroutineState {
    RunningNoParent {
        vm_frames: Vec<CallFrame>,
        vm_stack: Vec<RuaVal>,
    },
    RunningWithParent {
        parent: Rc<Coroutine>,
    },
    NormalNoParent {
        // Active, but called another coroutine
        own_frames: Vec<CallFrame>,
        own_stack: Vec<RuaVal>,
        vm_frames: Vec<CallFrame>,
        vm_stack: Vec<RuaVal>,
    },
    NormalWithParent {
        // Active, but called another coroutine
        own_frames: Vec<CallFrame>,
        own_stack: Vec<RuaVal>,
        parent: Rc<Coroutine>,
    },
    Suspended {
        head: Option<Callable>,
        own_frames: Vec<CallFrame>,
        own_stack: Vec<RuaVal>,
    },
    Dead,
}

#[derive(Debug)]
pub struct Coroutine {
    ip: Cell<usize>,
    state: RefCell<CoroutineState>,
}

impl Coroutine {
    pub fn new(head: Callable) -> Self {
        let stack_size = match &head {
            Callable::Closure(c) => usize::max(c.function().max_used_regs() as usize, 1),
            Callable::Native(_) => 0,
        };

        Self {
            ip: 0.into(),
            state: CoroutineState::Suspended {
                head: Some(head),
                own_frames: Vec::new(),
                own_stack: vec![RuaVal::nil(); stack_size],
            }
            .into(),
        }
    }

    pub(crate) fn addr(&self) -> usize {
        std::ptr::addr_of!(*self) as usize
    }

    #[inline]
    pub(crate) fn ip(&self) -> usize {
        self.ip.get()
    }

    pub(crate) fn resume(
        &self,
        vm: &mut Vm,
        current_frame: &mut CallFrame,
        retval_dst: u8,
        ret_ip: usize,
    ) -> usize {
        let state = &mut *self.state.borrow_mut();
        match state {
            CoroutineState::Suspended { head, own_frames, own_stack } => {
                let coroutine_frame = match head.take() {
                    Some(Callable::Closure(c)) => CallFrame::new(c, 0, 0, retval_dst, ret_ip), // TODO correct retval_dst
                    Some(Callable::Native(f)) => {
                        todo!();
                        // return vm.call_native(frame, &f, *args_start, *nargs, curr_ip);
                    }
                    None => own_frames.pop().expect("Empty coroutine should be dead"),
                };

                let vm_stack = std::mem::take(&mut vm.stack);
                let mut vm_frames = std::mem::take(&mut vm.frames);
                vm.stack = std::mem::take(own_stack);
                vm.frames = std::mem::take(own_frames);

                let old_frame = std::mem::replace(current_frame, coroutine_frame);
                vm_frames.push(old_frame);

                let new_state = match &vm.current_coroutine {
                    Some(parent_co) => {
                        parent_co.to_normal(vm_stack, vm_frames);
                        CoroutineState::RunningWithParent { parent: parent_co.clone() }
                    }
                    None => CoroutineState::RunningNoParent { vm_frames, vm_stack },
                };
                *state = new_state;

                self.ip()
            }
            state => unreachable!("Cannot resume coroutine in state {state:?}"),
        }
    }

    // Yes it's mispelled. No I don't care
    pub(crate) fn shield(&self) {
        let state = &mut *self.state.borrow_mut();
    }

    fn to_normal(&self, own_stack: Vec<RuaVal>, own_frames: Vec<CallFrame>) {
        let mut state_ref = self.state.borrow_mut();
        let state = std::mem::replace(&mut *state_ref, CoroutineState::Dead);
        *state_ref = match state {
            CoroutineState::RunningNoParent { vm_frames, vm_stack } => {
                CoroutineState::NormalNoParent { own_frames, own_stack, vm_frames, vm_stack }
            }
            CoroutineState::RunningWithParent { parent, .. } => {
                CoroutineState::NormalWithParent { own_frames, own_stack, parent }
            }
            state => unreachable!("Cannot move to normal coroutine with state {state:?}"),
        };
    }

    fn resume_from_normal(&self, vm: &mut Vm, current_frame: &mut CallFrame) {
        let mut state_ref = self.state.borrow_mut();
        let state = std::mem::replace(&mut *state_ref, CoroutineState::Dead);
        let (mut own_frames, own_stack) = match state {
            CoroutineState::NormalWithParent { own_frames, own_stack, parent } => {
                *state_ref = CoroutineState::RunningWithParent { parent };
                (own_frames, own_stack)
            }
            CoroutineState::NormalNoParent { own_frames, own_stack, vm_frames, vm_stack } => {
                *state_ref = CoroutineState::RunningNoParent { vm_frames, vm_stack };
                (own_frames, own_stack)
            }
            state => unreachable!("Parent of coroutine had state {state:?}"),
        };
        *current_frame = own_frames.pop().expect("Empty coroutine should be dead");
        vm.frames = own_frames;
        vm.stack = own_stack;
    }

    pub(crate) fn kill(&self, vm: &mut Vm, current_frame: &mut CallFrame) -> usize {
        let mut state = self.state.borrow_mut();

        let ip = match &mut *state {
            CoroutineState::RunningWithParent { parent } => {
                parent.resume_from_normal(vm, current_frame);
                vm.current_coroutine = Some(parent.clone());
                parent.ip()
            }
            CoroutineState::RunningNoParent { vm_frames, vm_stack } => {
                vm.frames = std::mem::take(vm_frames);
                vm.stack = std::mem::take(vm_stack);
                current_frame.ret_ip()
            }
            state => unreachable!("Cannot kill coroutine with state {state:?}"),
        };

        *current_frame = vm.frames.pop().expect("Empty coroutine should be dead");
        *state = CoroutineState::Dead;
        ip
    }

    pub fn is_dead(&self) -> bool {
        let state = &*self.state.borrow();
        matches!(state, CoroutineState::Dead)
    }

    pub fn status(&self) -> CoroutineStatus {
        let state = &*self.state.borrow();
        state.as_status()
    }
}

impl CoroutineState {
    fn as_status(&self) -> CoroutineStatus {
        match self {
            CoroutineState::RunningNoParent { .. } | CoroutineState::RunningWithParent { .. } => {
                CoroutineStatus::Running
            }
            CoroutineState::NormalNoParent { .. } | CoroutineState::NormalWithParent { .. } => {
                CoroutineStatus::Normal
            }
            CoroutineState::Suspended { .. } => CoroutineStatus::Suspended,
            CoroutineState::Dead => CoroutineStatus::Dead,
        }
    }
}

impl std::fmt::Display for CoroutineStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CoroutineStatus::Running => write!(f, "running"),
            CoroutineStatus::Normal => write!(f, "normal"),
            CoroutineStatus::Suspended => write!(f, "suspended"),
            CoroutineStatus::Dead => write!(f, "dead"),
        }
    }
}

impl PartialEq for Coroutine {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Eq for Coroutine {}

impl std::hash::Hash for Coroutine {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::addr_of!(*self).hash(state);
    }
}

impl IntoRuaVal for Coroutine {
    fn into_rua(self, vm: &mut Vm) -> RuaVal {
        Rc::new(self).into_rua(vm)
    }
}

impl IntoRuaVal for Rc<Coroutine> {
    fn into_rua(self, vm: &mut Vm) -> RuaVal {
        // TODO register in vm
        RuaVal(RuaValInner::Coroutine(self))
    }
}
