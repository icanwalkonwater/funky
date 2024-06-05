use std::{fs::File, io::Read, path::PathBuf};

use pest::Parser;

#[derive(Debug, clap::Parser)]
struct CliOpts {
    filename: PathBuf,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opts = <CliOpts as clap::Parser>::parse();

    let mut source_file = File::open(&opts.filename)?;
    let mut code = String::new();
    source_file.read_to_string(&mut code)?;

    let res = funky::FunkyParser::parse(funky::Rule::File, &code);
    dbg!(&res);

    Ok(())
}
