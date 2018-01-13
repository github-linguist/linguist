// Kojo examples (files ending in .kojo) are licensed under The MIT License:

// Copyright (C) 2009-2018 Lalit Pant <pant.lalit@gmail.com> and the Kojo Dev Team.

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.


// Example from http://lalitpant.blogspot.in/2012/05/recursive-drawing-with-kojo.html
// This example is based on Kojo Pictures
val size = 100
def S = Picture {
    repeat (4) {
        forward(size)
        right()
    }
}

def stem = scale(0.13, 1) * penColor(noColor) * fillColor(black) -> S

clear()
setBackground(Color(255, 170, 29))
invisible()

def drawing(n: Int): Picture = {
    if (n == 1) 
        stem
    else 
        GPics(stem,
              trans(2, size-5) * brit(0.05) -> GPics(
                rot(25) * scale(0.72) -> drawing(n-1),
                rot(25) * trans(0, size * 0.72) * rot(-75) * scale(0.55) -> drawing(n-1)
            )
        )
}

val pic = trans(0, -100) -> drawing(10)
draw(pic)