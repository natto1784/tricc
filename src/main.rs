use std::{
    fs,
    panic,
};

use tricc::args::Args;
use tricc::lexer::{
    Lexer,
    TokenKind,
};

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

    let mut args = Args::default();
    args.handle();

    let file = args.get_file();
    let content = fs::read_to_string(&file).expect("Couldn't read the file");

    let mut lexer = Lexer::new(content.as_str());

    while lexer.peek_token().kind != TokenKind::Eof {
        println!("{:?}", lexer.next_token());
    }
}
