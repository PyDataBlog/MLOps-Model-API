use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

enum Lang {
    Python,
    Unknown,
}

struct CommentDelimiters {
    begin: &'static str,
    end: &'static str,
}

fn main() {
    let fname = env::args().nth(1).unwrap();
    let lang = lang_guess(&fname);
    let delims = comment_delims(lang);
    let path = Path::new(&fname);
    let f = File::open(path);
    match f {
        Ok(mut fr) => {
            let mut s = String::new();
            fr.read_to_string(&mut s);
            let v = reduce_to_comments(s, delims);
            println!("_");
        },
        Err(_) => { },
    }    
}

fn lang_guess(s: &String) -> Lang {
    let path = Path::new(&s);
    let ext = path.extension();
    match ext {
        Some(x) => {
            let y = x.to_str().unwrap();
            match y {
                "py" => Lang::Python,
                _ => Lang::Unknown,
            }
        },
        None => Lang::Unknown,
    }
}

fn comment_delims(lang: Lang) -> CommentDelimiters {
    match lang {
        Lang::Python => CommentDelimiters { begin: "#", end: "\n" },
        _ => CommentDelimiters { begin: "", end: "" },
    }
}

fn reduce_to_comments(s: String, ds: CommentDelimiters) -> Vec<String> {
    vec![]
}
