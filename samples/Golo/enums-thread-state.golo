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

module sample.EnumsThreadState

import java.lang.Thread$State

function main = |args| {

  # Call the enum entry like a function
  let new = Thread$State.NEW()
  println("name=" + new: name() + ", ordinal=" + new: ordinal())
  println("-----------")

  # Walk through all enum entries
  foreach element in Thread$State.values() {
    println("name=" + element: name() + ", ordinal=" + element: ordinal())
  }
}

