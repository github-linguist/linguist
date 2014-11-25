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

module samples.MemoizeDecorator

import gololang.Decorators

import java.lang.System

let memo = memoizer()

@memo
function fib = |n| {
  if n <= 1 {
    return n
  } else {
    return fib(n - 1) + fib(n - 2)
  }
}

@memo
function foo = |n| -> n

local function run = {
  let start = System.currentTimeMillis()
  let result = fib(40)
  let duration = System.currentTimeMillis() - start
  println(">>> fib(40) = " + result + " (took " + duration + "ms)")
}

local function run2 = {
  let start = System.currentTimeMillis()
  let result = foo(40)
  let duration = System.currentTimeMillis() - start
  println(">>> foo(40) = " + result + " (took " + duration + "ms)")
}

function main = |args| {
  foreach i in range(0, 5) {
    println("run " + i)
    run()
    run2()
  }
}
