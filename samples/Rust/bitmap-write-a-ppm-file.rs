use std::vec::from_elem;
use std::path::posix::{Path};
use std::io::File;

pub struct RGB {
  r: u8
 ,g: u8
 ,b: u8
}

pub struct PPM {
  height: uint
 ,width: uint
 ,data: ~[u8]
}

impl PPM {
  pub fn new(height: uint, width: uint) -> PPM {
    let size = 3 * height * width;
    let buffer = from_elem(size, 0u8);
    PPM{height: height, width: width, data: buffer}
  }

  fn buffer_size(&self) -> uint {
    3 * self.height * self.width
  }

  fn get_offset(&self, x: uint, y: uint) -> Option<uint> {
    let offset = (y * self.width * 3) + (x * 3);
    if(offset < self.buffer_size()){
      Some(offset)
    }else{
      None
    }
  }

  pub fn get_pixel(&self, x: uint, y: uint) -> Option<RGB> {
    match self.get_offset(x, y) {
      Some(offset) => {
        let r = self.data[offset];
        let g = self.data[offset + 1];
        let b = self.data[offset + 2];
        Some(RGB{r, g, b})
      },
      None => None
    }
  }

  pub fn set_pixel(&mut self, x: uint, y: uint, color: RGB) -> bool {
    match self.get_offset(x, y) {
      Some(offset) => {
        self.data[offset] = color.r;
        self.data[offset + 1] = color.g;
        self.data[offset + 2] = color.b;
        true
      },
      None => false
    }
  }

  pub fn write_file(&self, filename: &str) -> bool {
    let path = Path::new(filename);
    let mut file = File::create(&path);
    let header = format!("P6 {} {} 255\n", self.width, self.height);
    file.write(header.as_bytes());
    file.write(self.data);
    true
  }

}
