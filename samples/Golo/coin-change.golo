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

module CoinChange

import java.util.LinkedList

function change = |money, coins| -> match {
  when money == 0 then 1
  when (money < 0) or (coins: isEmpty()) then 0
  otherwise change(money - coins: head(), coins) + change(money, coins: tail())
}

function main = |args| {
  let coins = LinkedList(): append(1, 2, 5, 10, 20)
  println("Coins: " + coins)
  println("0: " + change(0, coins))
  println("1: " + change(1, coins))
  println("2: " + change(2, coins))
  println("10: " + change(10, coins))
  println("12: " + change(12, coins))
  println("6: " + change(6, coins))
}
