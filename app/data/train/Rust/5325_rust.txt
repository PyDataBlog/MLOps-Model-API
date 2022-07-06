extern crate libc;

///! Rush main

/// Rush module
mod rush
{
    use std::ptr;
    use std::io::stdio;
    use libc::funcs::posix88::unistd::execvp;
    use libc::funcs::posix88::unistd::fork;
    use libc::types::os::arch::posix88::pid_t;

    /// Wait.
    #[link(name = "c")]
    extern
    {
        fn wait (status: * mut i64) -> pid_t;
    }

    /// Print the prompt.
    fn print_prompt ()
    {
        let mut stdout = stdio::stdout ();
        let _ = stdout.write_str ("rush >> ");
        let _ = stdout.flush ();
    }

    /// Read user's input into a buffer.
    fn read_input (buffer: & mut [u8]) -> uint
    {
        let mut stdin = stdio::stdin ();
        let n = match stdin.read (buffer)
        {
            Ok (n) => n,
            Err (..) => 0,
        };
        n
    }

    /// Tokenize a buffer (split at spaces).
    fn buffer_to_tokens (n: uint, buffer: & mut [u8]) -> Vec<String>
    {
        let input_string = match String::from_utf8 (Vec::from_slice (buffer))
        {
            Ok (s) => s,
            Err (e) => fail! ("{}", e),
        };
        let tokens: Vec<&str> = input_string.as_slice().slice (0, n-1).split(' ').collect();
        let mut tokens_strings = Vec::new ();
        for s in tokens.iter ()
        {
            tokens_strings.push (String::from_str (*s));
        }
        tokens_strings
    }

    /// Run a command.
    fn run_cmd (cmd: & str, args: Vec<String>) -> pid_t
    {
        unsafe
        {
            let pid = fork ();
            if pid == 0
            {
                let cmd_c_str = cmd.to_c_str ();
                let arg_0 = cmd_c_str.clone ();
                let mut args_c : Vec<* const i8> = Vec::new ();
                args_c.push (arg_0.unwrap ());
                for s in args.iter ()
                {
                    args_c.push (s.to_c_str ().unwrap ());
                }
                args_c.push (ptr::null ());
                execvp (cmd_c_str.unwrap (), args_c.as_mut_ptr ());
            }
            pid
        }
    }

    /// Run the prompt.
    pub fn prompt ()
    {
        loop
        {
            print_prompt ();

            /* Read input. */
            let mut buffer : [u8, ..2048] = [0, ..2048];
            let n = read_input (buffer);
            let input = buffer_to_tokens (n, buffer);

            let cmd = input.as_slice ()[0].as_slice ();
            if cmd == "exit"
            {
                break;
            }

            let args = Vec::from_slice (input.as_slice ().slice (1, input.len ()));

            let pid = run_cmd (cmd, args);
            if pid == 0
            {
                break;
            }
            let mut status: i64 = 0;
            unsafe
            {
                wait (&mut status);
            }
        }
    }
}

/// Main
fn main ()
{
    rush::prompt ();
}
