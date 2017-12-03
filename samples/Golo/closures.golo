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

module Closures

local function sayHello = |who| -> "Hello " + who + "!"

function main = |args| {
  let adder = |a, b| -> a + b
  println(adder: invokeWithArguments(1, 2))
  println(adder(1, 2))

  let addToTen = adder: bindTo(10)
  println(addToTen: invokeWithArguments(2))
  println(addToTen(2))

  let adding = |x| -> |y| -> adder(x, y)
  let addingTen = adding(10)
  println(addingTen(4))
  println(adding(2)(4))

  println(sayHello("Julien"))

  let list = java.util.LinkedList()
  let pump_it = {
    list: add("I heard you say")
    list: add("Hey!")
    list: add("Hey!")
  }
  pump_it()
  println(list)
}
