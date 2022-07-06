use piston::*;

pub struct App {
    objects: Vec<()>,
}

impl App {
    pub fn new() -> App {
        App {
            objects: Vec::new(),
        }
    }
}

impl Game for App {

}

