use std::env;
use std::path::PathBuf;
use std::process::exit;

const VERSION: &str = env!("CARGO_PKG_VERSION");
const CRATE: &str = env!("CARGO_CRATE_NAME");

/// A naive argument handler
#[derive(Default)]
pub struct Args {
    version: bool,
    file: Option<PathBuf>,
}

impl Args {
    /// Creates a new [`Args`] instance
    pub fn new() -> Args {
        Args::default()
    }

    /// Checks for various arguments
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
                    self.file = Some(PathBuf::from(file));
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

    /// Fetches the file from the arguments.
    /// Panics if there is no file in the arguments
    #[inline]
    pub fn get_file(self) -> PathBuf {
        self.file.expect("no file supplied!")
    }
}
