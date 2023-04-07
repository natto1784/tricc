use std::fs;
use std::panic;

use tricc::args::Args;

fn main() {
    panic::set_hook(Box::new(|panic_info| {
        if let Some(msg) = panic_info.payload().downcast_ref::<&str>() {
            eprintln!("{}", msg);
        } else if let Some(msg) = panic_info.payload().downcast_ref::<String>() {
            eprintln!("{}", msg);
        } else if let Some(location) = panic_info.location() {
            eprintln!(
                "panic occurred in file '{}' at line {}",
                location.file(),
                location.line(),
            );
        } else {
            eprintln!("panic occurred");
        }
    }));

    let mut args = Args::new();
    args.handle();

    let file = args.get_file();
    let contents = fs::read_to_string(&file).expect("Couldn't read the file");
}
