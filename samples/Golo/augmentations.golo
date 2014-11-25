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

module samples.Augmentations

import java.util.LinkedList

augment java.util.List {
  function with = |this, value| {
    this: add(value)
    return this
  }
}

augment java.util.Collection {
  function doToEach = |this, func| {
    foreach (element in this) {
      func(element)
    }
  }
}

function main = |args| {
  let list = LinkedList(): with("foo"): with("bar"): with("baz")
  list: doToEach(|value| -> println(">>> " + value))
}
