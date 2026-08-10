// Copyright 2024 International Digital Economy Academy
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Origin: https://github.com/moonbit-community/moonbit-native-benchmark1/tree/master

pub struct Complex {
  real : Double
  imag : Double
}

pub fn Complex::add(self : Complex, other : Complex) -> Complex {
  { real: self.real + other.real, imag: self.imag + other.imag }
}

pub fn Complex::sub(self : Complex, other : Complex) -> Complex {
  { real: self.real - other.real, imag: self.imag - other.imag }
}

pub fn Complex::mul(self : Complex, other : Complex) -> Complex {
  {
    real: self.real * other.real - self.imag * other.imag,
    imag: self.real * other.imag + self.imag * other.real,
  }
}

pub fn Complex::mul_scalar(self : Complex, scalar : Double) -> Complex {
  { real: self.real * scalar, imag: self.imag * scalar }
}

/// The function implementing the Cooley–Tukey algorithm for Fast Fourier Transform (FFT), as
/// described in <https://cp-algorithms.com/algebra/fft.html#implementation>.
///
/// The input signal is modified in-place.
pub fn fft(arr : Array[Complex]) -> Unit {
  fn _fft(arr : Array[Complex]) {
    let n = arr.length()
    if n == 1 {
      return
    }
    let a0 = Array::new(capacity=n / 2)
    let a1 = Array::new(capacity=n / 2)
    for i in 0..<(n / 2) {
      a0.push(arr[2 * i])
      a1.push(arr[2 * i + 1])
    }
    _fft(a0)
    _fft(a1)
    let ang = -2.0 * @math.pi / Double::from_int(n)
    let mut w = { real: 1.0, imag: 0.0 }
    let wn = { real: @math.cos(ang), imag: @math.sin(ang) }
    for i in 0..<(n / 2) {
      let p = a0[i]
      let q = w.mul(a1[i])
      arr[i] = p.add(q)
      arr[i + n / 2] = p.sub(q)
      w = w.mul(wn)
    }
  }

  _fft(arr)
  let factor = 1.0 / Double::from_int(arr.length()).sqrt()
  for i in 0..<arr.length() {
    arr[i] = arr[i].mul_scalar(factor)
  }
}

typealias Complex = @lib.Complex

fn main {
  let signals = generate_inputs(16384)
  @lib.fft(signals)
  for signal in signals {
    println("\{round(signal.real)},\{round(signal.imag)}")
  }
}

fn round(n : Double) -> Double {
  // precision = 2
  (n * 100.0).round() / 100.0
}

fn generate_inputs(len : Int) -> Array[Complex] {
  let res : Array[Complex] = []
  for i in 0..<len {
    let theta = i.to_double() / len.to_double() * @math.pi
    let re = 1.0 * @math.cos(10.0 * theta) + 0.5 * @math.cos(25.0 * theta)
    let im = 1.0 * @math.sin(10.0 * theta) + 0.5 * @math.sin(25.0 * theta)
    res.push({ real: round(re), imag: round(im) })
  }
  res
}
