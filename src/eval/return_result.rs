use super::vals::RuaVal;

pub(super) enum ReturnResult {
    BigReturn(RuaVal),
    SmallReturn(usize),
}

impl ReturnResult {
    pub(super) fn map_small<F: FnOnce(usize) -> usize>(self, f: F) -> Self {
        match self {
            r @ Self::BigReturn(_) => r,
            Self::SmallReturn(ip) => Self::SmallReturn(f(ip)),
        }
    }
}
