use std::process::Command;

// shell

static FILE_CMD :& 'static str = "file";

fn main() {

    let output = Command::new(FILE_CMD)
            .arg("--version1")
            .output().unwrap_or_else( |e|
                panic!("failed to run cmd: {} with {}", FILE_CMD, e)
            );


    if output.status.success() {
        println!("stdout: {}", String::from_utf8_lossy(&output.stdout));
    } else {
        println!("stderr: {}", String::from_utf8_lossy(&output.stderr));
    }

}