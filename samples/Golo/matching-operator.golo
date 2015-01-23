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

module Matching

import java.util.LinkedList

local function data = {
  let list = LinkedList()
  list: add("foo@bar.com")
  list: add("+33.6.11.22.33")
  list: add("http://golo-lang.org/")
  list: add("def foo = bar(_._) with :> T")
  return list
}

local function what_it_could_be = |item| -> match {
  when item: contains("@") then "an email?"
  when item: startsWith("+33") then "a French phone number?"
  when item: startsWith("http://") then "a website URL?"
  otherwise "I have no clue, mate!"
}

function main = |args| {
  foreach item in data() {
    println(item + " => " + what_it_could_be(item))
  }
}

