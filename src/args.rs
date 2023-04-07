use std::env;
use std::process::exit;

const VERSION: &str = env!("CARGO_PKG_VERSION");
const CRATE: &str = env!("CARGO_CRATE_NAME");

// naive argument handling
#[derive(Default)]
pub struct Args {
    version: bool,
    file: Option<String>,
}

impl Args {
    pub fn new() -> Args {
        Args {
            version: false,
            file: None,
        }
    }

    pub fn handle(&mut self) {
        let args: Vec<String> = env::args().collect();

        if args.len() < 2 {
            println!("Usage: {} [-v] <file>", CRATE);
            exit(0);
        }

        for arg in &args[1..] {
            match arg.as_str() {
                "-v" | "--version" => self.version = true,
                flag if flag.starts_with('-') => panic!("option {} not implemented!", flag),
                file => {
                    if self.file.is_some() {
                        panic!("please specify only a single source file!");
                    }
                    self.file = Some(file.to_owned());
                }
            }
        }

        if self.version {
            println!("{} version: {}", CRATE, VERSION);
        }

        if self.file.is_none() {
            if self.version {
                exit(0);
            } else {
                panic!("no file supplied!");
            }
        }
    }

    #[inline]
    pub fn get_file(self) -> String {
        self.file.expect("no file supplied!")
    }
}
