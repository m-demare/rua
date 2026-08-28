macro_rules! trace_gc {
    ($($arg: tt)*) => {
        #[cfg(any(test, clippy))]
        println!($($arg)*);
    };
}

pub(super) use trace_gc;
