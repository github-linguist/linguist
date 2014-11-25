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

module MoreCoolContainers

function main = |args| {
  
  println(">>> DynamicVariable")

  let dyn = DynamicVariable("Foo")
  println(dyn: value())
  
  let t1 = Thread({
  dyn: withValue(666, {
      println(dyn: value())
    })
  })
  
  let t2 = Thread({
    dyn: withValue(69, {
      println(dyn: value())
    })
  })

  t1: start()
  t2: start()
  t1: join()
  t2: join()
  println(dyn: value())

  println(">>> Observable")

  let foo = Observable("Foo")
  foo: onChange(|v| -> println("foo = " + v))

  let mapped = foo: map(|v| -> v + "!")
  mapped: onChange(|v| -> println("mapped = " + v))
  
  foo: set("69")
}
