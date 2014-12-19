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

module samples.Adapters

local function list_sample = |fabric| {
  println(">>> list_sample()")
  let carbonCopy = list[]
  let conf = map[
    ["extends", "java.util.ArrayList"],
    ["overrides", map[
      ["*", |super, name, args| {
        if name == "add" {
          if args: length() == 2 {
            carbonCopy: add(args: get(1))
          } else {
            carbonCopy: add(args: get(1), args: get(2))
          }
        }
        return super: invokeWithArguments(args)
      }
    ]]
  ]]
  let list = fabric: maker(conf): newInstance()
  list: add("bar")
  list: add(0, "foo")
  list: add("baz")
  println("      list: " + list + " " + list: getClass())
  println("carbonCopy: " + carbonCopy + " " + carbonCopy: getClass())
}

local function runnable_sample = |fabric| {
  println(">>> runnable_sample")
  let result = array[1, 2, 3]
  let conf = map[
    ["interfaces", ["java.io.Serializable", "java.lang.Runnable"]],
    ["implements", map[
      ["run", |this| {
        for (var i = 0, i < result: length(), i = i + 1) {
          result: set(i, result: get(i) + 10)
        }
      }]
    ]]
  ]
  let runner = fabric: maker(conf): newInstance()
  runner: run()
  println("      result: " + result: toString())
  println("serializable? " + (runner oftype java.io.Serializable.class))
  println("    runnable? " + (runner oftype java.lang.Runnable.class))
}

function main = |args| {
  let fabric = AdapterFabric()
  list_sample(fabric)
  runnable_sample(fabric)
}
