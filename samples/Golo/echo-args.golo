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

module EchoArgs

function main = |args| {
  
  println("With a for loop and an index:")
  for (var i = 0, i < args: length(), i = i + 1) {
    println("  #" + i + " -> " + args: get(i))
  }

  println("With a foreach loop:")
  foreach arg in args {
    println("  " + arg)
  }

  println("With a foreach over a range:")
  foreach i in range(0, args: length()) {
    println("  #" + i + " -> " + args: get(i))
  }
}

