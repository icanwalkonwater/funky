use std::{fs::File, path::PathBuf};

use clap::Parser;
use funky::lexer::tokenize;

#[derive(Debug, clap::Parser)]
struct CliOpts {
    filename: PathBuf,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opts = CliOpts::parse();
    println!("Parsing entry point at {}", opts.filename.display());

    let tokens = tokenize(File::open(opts.filename)?)?;
    dbg!(tokens);
    Ok(())
}
