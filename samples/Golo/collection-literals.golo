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

module samples.CollectionLiterals

local function play_with_tuples = {
  let hello = ["Hello", "world", "!"]
  foreach str in hello {
    print(str + " ")
  }
  println("")

  println(hello: get(0) + "-" + hello: get(1) + "-" + hello: get(2))

  println(hello: join("/"))
}

local function play_with_literals = {
  let data = [
    [1, 2, 3],
    tuple[1, 2, 3],
    array[1, 2, 3],
    set[1, 2, 3, 3, 1],
    map[
      ["a", 10],
      ["b", 20]
    ],
    vector[1, 2, 3],
    list[1, 2, 3]
  ]

  data: each(|element| {
    println(element: toString())
    println("  type: " + element: getClass())
  })
}

function main = |args| {
  println(">>> Literals")
  play_with_literals()
  println("\n>>> Tuples")
  play_with_tuples()
}

