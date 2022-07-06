
use std::path::PathBuf;
use std::process;

pub fn ensure_file_exists(path: &PathBuf) {

    if !path.exists() {
        eprintln!("the given path does not exist: {:?}", path);
        process::exit(1);
    }

    if !path.is_file() {
        eprintln!("the given path is not a file: {:?}", path);
        process::exit(1);
    }

}