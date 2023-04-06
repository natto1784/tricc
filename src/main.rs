use tricc::args;

fn main() {
    let file: String = args::handle();
    println!("{}", file);
}
