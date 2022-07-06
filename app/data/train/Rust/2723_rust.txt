pub enum ColorName {
    Black,
    Blue,
    Red,
    Purple,
    Green,
    Cyan,
    Yellow,
    White,
}

pub fn number_to_color(num: u8) -> ColorName {
    match num {
        0 => ColorName::Black,
        1 => ColorName::Blue,
        2 => ColorName::Red,
        3 => ColorName::Purple,
        4 => ColorName::Green,
        5 => ColorName::Cyan,
        6 => ColorName::Yellow,
        _ => ColorName::White,
    }
}

pub struct Color {
    blue: bool,
    red: bool,
    green: bool,
    pub value: u8,
}

fn trf(val: bool) -> u16 {
    if val { 4095 } else { 0 }
}

impl Color {
    pub fn new(name: ColorName) -> Color {
        let value = name as u8;
        Color {
            blue: (value & 1) > 0,
            red: (value & 2) > 0,
            green: (value & 4) > 0,
            value: value,
        }
    }
    
    pub fn to_array(&self) -> [u16; 3] {
       [trf(self.blue), trf(self.red), trf(self.green)]
    }
}