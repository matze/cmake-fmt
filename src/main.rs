use crate::formatter::Format;
use clap::Parser;
use std::path::PathBuf;

mod formatter;
mod parser;

#[derive(Parser)]
struct Args {
    /// Path to a CMakeLists.txt file
    #[arg(default_value = "CMakeLists.txt")]
    filename: PathBuf,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    let content = std::fs::read_to_string(args.filename)?;
    let nodes = parser::parse(&content)?;
    println!("{}", nodes.format(&mut formatter::Context::default()));

    Ok(())
}
