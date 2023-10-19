use std::path::PathBuf;

#[derive(Debug)]
pub struct Cli {
    pub path: Option<PathBuf>,
}

pub fn parse_args() -> Cli {
    Cli { path: std::env::args().nth(1).map(PathBuf::from) }
}
