use std::path::PathBuf;

use clap::Parser;

/// Rua language interpreter
#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
pub struct CliArgs {
    #[arg()]
    pub(super) path: Option<PathBuf>,

    /// Print disassembly
    #[arg(short, long, default_value_t = false)]
    pub(super) list: bool,

    /// Skip script evaluation
    #[arg(short, long = "parse-only", default_value_t = false)]
    pub(super) parse_only: bool,

    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    pub(super) args: Vec<Vec<u8>>,
}
