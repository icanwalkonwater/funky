use std::{fs::File, io::Read, path::PathBuf};

use funky::Rule;
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

    let [file] = funky::FunkyParser::parse(Rule::File, &code)?
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();

    assert_eq!(file.as_rule(), Rule::File);
    let file = funky::parser::parse_file(file);

    dbg!(file);

    Ok(())
}
