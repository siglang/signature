use clap::Parser;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[clap(bin_name = "sntkc", version = "0.0.0", arg_required_else_help = true)]
pub struct Cli {
    #[arg(help = "The source file to compile and run")]
    pub source: PathBuf,
    #[arg(short, long, help = "Use debug output")]
    pub debug: bool,
    #[arg(short, long, help = "Use verbose output")]
    pub verbose: bool,
    #[arg(short, long, value_name = "FILE", help = "Use a custom config file")]
    pub config: Option<PathBuf>,
}
