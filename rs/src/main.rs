use std::env;
use std::fs;

fn main() {
    for argument in env::args().skip(1) {
        let contents =
            fs::read_to_string(&argument).expect(&format!("Error reading {:?}", &argument));
        for line in contents.lines() {
            println!("{:?}", line);
        }
    }
}
