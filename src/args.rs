use std::env;
use std::process::exit;

const VERSION: &'static str = env!("CARGO_PKG_VERSION");
const CRATE: &'static str = env!("CARGO_CRATE_NAME");

#[derive(Default)]
struct Options {
    version: bool,
    file: String,
}

// naive argument handling
pub fn handle() -> String {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Usage: {} [-v] <file>", CRATE);
        exit(0);
    }

    let mut options: Options = Default::default();

    for arg in &args[1..] {
        match arg.as_str() {
            "-v" | "--version" => options.version = true,
            flag if flag.chars().next() == Some('-') => panic!("option {} not implemented!", flag),
            file @ _ => {
                if !options.file.is_empty() {
                    panic!("please specify only a single source file!");
                }
                options.file = file.to_string();
            }
        }
    }

    if options.version {
        println!("{} version: {}", CRATE, VERSION);
    }

    if options.file.is_empty() {
        if options.version {
            exit(0);
        } else {
            panic!("no file supplied!");
        }
    }

    options.file
}
