macro_rules! trace_gc {
    ($($arg: tt)*) => {
        #[cfg(test)]
        println!($($arg)*);
    };
}

pub(super) use trace_gc;
