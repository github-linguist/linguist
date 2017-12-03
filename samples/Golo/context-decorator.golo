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

module samples.ContextDecorator

import gololang.Decorators

let myContext = defaultContext():
  count(0):
  define("entry", |this, args| {
    this: count(this: count() + 1)
    println("hello:" + this: count())
    return args
  }):
  define("exit", |this, result| {
    require(result >= 3, "wrong value")
    println("goobye")
    return result
  }):
  define("catcher", |this, e| {
    println("Caught " + e)
    throw e
  }):
  define("finallizer", |this| {println("do some cleanup")})


@withContext(myContext)
function foo = |a, b| {
  println("Hard computation")
  return a + b
}

function main = |args| {
  println(foo(1,2))
  println("====")
  println(withContext(myContext)(|a| -> 2*a)(3))
  println("====")
  try {
    println(foo(1, 1))
  } catch (e) { }
}

