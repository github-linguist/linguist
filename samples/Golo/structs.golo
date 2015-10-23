# Copyright 2012-2014 Institut National des Sciences Appliquées de Lyon (INSA-Lyon)
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

module StructDemo

struct Point = { x, y }

augment StructDemo.types.Point {

  function move = |this, offsetX, offsetY| {
    this: x(this: x() + offsetX)
    this: y(this: y() + offsetY)
    return this
  }

  function relative = |this, offsetX, offsetY| -> Point(this: x() + offsetX, this: y() + offsetY)
}

function main = |args| {
  
  let p1 = Point(1, 2)
  let p2 = Point(): x(1): y(2)
  let p3 = p1: frozenCopy()
  let p4 = p1: frozenCopy()

  println(p1)
  println("x = " + p1: x())
  println("y = " + p1: y())

  println("p1 == p2 " + (p1 == p2))
  println("p1 == p3 " + (p1 == p3))
  println("p3 == p4 " + (p3 == p4))

  println("#p1 " + p1: hashCode())
  println("#p2 " + p2: hashCode())
  println("#p3 " + p3: hashCode())
  println("#p4 " + p4: hashCode())

  println("p1: members() " + p1: members())
  println("p1: values() " + p1: values())
  foreach item in p1 {
    println(item: get(0) + " -> " + item: get(1))
  }

  println("p1: set(\"x\", 10) " + p1: set("x", 10))
  println("p1: move(10, 5) " + p1: move(10, 5))
  println("p1: relative(11, 6) " + p1: relative(11, 6))

  let p5 = ImmutablePoint(10, 20)
  println("p5: " + p5)
  try {
    p5: x(100)
  } catch (expected) {
    println("p5 is immutable, so... " + expected: getMessage())
  }
}


