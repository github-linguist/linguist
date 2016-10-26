/** The MIT License (MIT)

Copyright (c) 2014 Hongwei Xi

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.*/

// Source: https://github.com/githwxi/ATS-Postiats-contrib/blob/master/contrib/arduino/CATS/Arduino.cats


/*
** The prelude for Ardunio
*/

/* ****** ****** */

#ifndef ARDUINO_CATS_ARDUINO
#define ARDUINO_CATS_ARDUINO

/* ****** ****** */

#include <Arduino.h>

/* ****** ****** */

#define delay_int(ms) delay(ms)
#define delay_ulint(ms) delay(ms)

/* ****** ****** */
//
#define random_int_1(x) random(x)
#define random_int_2(x, y) random(x, y)
#define random_lint_1(x) random(x)
#define random_lint_2(x, y) random(x, y)
//
#define randomSeed_int(x) randomSeed(x)
#define randomSeed_uint(x) randomSeed(x)
//
/* ****** ****** */

#endif // #ifndef(ARDUINO_CATS_ARDUINO)

/* ****** ****** */

/* end of [Arduino.cats] */
