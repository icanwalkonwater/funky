use std::{fs::File, path::PathBuf};

use clap::Parser;

#[derive(Debug, clap::Parser)]
struct CliOpts {
    filename: PathBuf,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opts = CliOpts::parse();
    println!("Parsing entry point at {}", opts.filename.display());
    Ok(())
}
