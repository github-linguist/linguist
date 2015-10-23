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

module Workers

import java.lang.Thread
import java.util.concurrent
import gololang.concurrent.workers.WorkerEnvironment

local function pusher = |queue, message| -> queue: offer(message)

local function generator = |port, message| {
  foreach i in range(0, 100) {
    port: send(message)
  }
}

function main = |args| {

  let env = WorkerEnvironment.builder(): withFixedThreadPool()
  let queue = ConcurrentLinkedQueue()

  let pusherPort = env: spawn(^pusher: bindTo(queue))
  let generatorPort = env: spawn(^generator: bindTo(pusherPort))

  let finishPort = env: spawn(|any| -> env: shutdown())

  foreach i in range(0, 10) {
    generatorPort: send("[" + i + "]")
  }
  Thread.sleep(2000_L)
  finishPort: send("Die!")

  env: awaitTermination(2000)
  println(queue: reduce("", |acc, next| -> acc + " " + next))
}

