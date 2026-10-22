/*
Source: https://github.com/obselate/goo/blob/34ed6f6e883edb88d2508b5d086d08f94e91532c/Goo/ArrayStorage.gs
License: https://github.com/obselate/goo/blob/34ed6f6e883edb88d2508b5d086d08f94e91532c/LICENSE

MIT License

Copyright (c) 2026 Xaz

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
SOFTWARE.
*/

package Goo

import System

internal func ArrayGrowthCapacity(current int32, required int32,
  minimum int32) int32 {
    if required < 0 { throw ArgumentOutOfRangeException("required") }
    if required <= current { return current }
    var capacity = Math.Max(current, minimum)
    while capacity < required {
      if capacity > Int32.MaxValue / 2 { return required }
      capacity *= 2
    }
    return capacity
  }

internal func GrowArray[T any](values []T, count int32, required int32,
  minimum int32) []T {
    let capacity = ArrayGrowthCapacity(values.Length, required, minimum)
    if capacity == values.Length { return values }
    let expanded = [capacity]T
    Array.Copy(values, expanded, count)
    return expanded
  }
