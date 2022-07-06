use std::os;
use std::io::File;
//use std::ops::BitXor;

fn main()
{
	let args: ~[~str] = os::args();

	if args.len() != 3
	{
		println!("Usage: {:s} <intputfile>", args[0]);
	}

	else
	{
		let fname_a = args[1].clone();
		let path_a = Path::new(fname_a.clone());
		let msg_file_a = File::open(&path_a);

		let fname_b = args[2];
		let path_b = Path::new(fname_b.clone());
		let msg_file_b = File::open(&path_b);

		match(msg_file_a, msg_file_b)
		{
			(Some(mut msg_a), Some(mut msg_b)) =>
			{
				let msg_bytes_a: ~[u8] = msg_a.read_to_end();
				let msg_bytes_b: ~[u8] = msg_b.read_to_end();
				let join_file = File::create(&Path::new("join.txt"));

				match (join_file)
				{
					Some(join) =>
					{
						joiner(join, msg_bytes_a, msg_bytes_b);
					},

					None => fail!("Error opening output files!"),
				}
			},

			(_, _) => fail!("Error opening message file: {:s}", fname_a)
		}

	}
}

fn xor(a: &[u8], b: &[u8])-> ~[u8]
{
	let mut ret = ~[];
	for i in range(0, a.len())
	{
		ret.push(a[i] ^ b[i]);
	}

	ret
}

fn joiner(mut join: File, msg_bytes_a: &[u8], msg_bytes_b: &[u8])
{
	let unencrypted_bytes = xor(msg_bytes_a, msg_bytes_b);
	join.write((unencrypted_bytes));
}