use std::io::File;

fn main(){
  let mut file = File::create(&Path::new("test.txt"));
  file.write(bytes!("pewpewpew\n"));
}
