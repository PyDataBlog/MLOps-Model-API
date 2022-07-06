/// Purpose of this binary is to update the submodule to a particular tag
/// TODO Could add it in that it will perform a check on the tags if it needs to be updated
// use std::env;
use std::process::Command;
fn main() {
    // let tag = env::args().nth(1);
    // if tag.is_none() {
    //     println!("Tag must be specificed on command line");
    // }
    // let tag = tag.unwrap();
    // println!("Updating the comms-spec submodule to version {:?}", tag);
    println!("Updating submodule comms-spec", );

    Command::new("git")
        .arg("submodule")
        .arg("update")
        .arg("--remote")
        .arg("comms-spec")
        .output()
        .expect("Got no output :(");

}
