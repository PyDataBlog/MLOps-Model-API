extern crate rustc_serialize;
use rustc_serialize::json;

#[derive(RustcDecodable, RustcEncodable)]
struct Command {
	cid: u64,
	uid: u32,
	target: String,
}

fn get_string(cmd: &Command) -> String {
	return json::encode(&cmd).unwrap();
}

fn main(){
	let bs = Command{
		cid: 0,
		uid: 1,
		target: "something".to_string()
	};
	println!("So guess what?: {}", get_string(&bs));
}

#[test]
fn some_test() {
	assert_eq!(
		get_string(
			&Command{cid: 0, uid: 1, target: "cat".to_string()}
		), "{\"cid\":0,\"uid\":1,\"target\":\"cat\"}");
}

#[test]
#[should_panic]
fn some_fail() {
	assert!(false);
}
