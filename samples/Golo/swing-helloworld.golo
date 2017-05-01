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

module samples.SwingHelloWorld

import javax.swing
import javax.swing.WindowConstants

function main = |args| {

  let frame = JFrame("Hello world")
  frame: setDefaultCloseOperation(EXIT_ON_CLOSE())

  let label = JLabel("Hello world")
  label: setFont(label: getFont(): deriveFont(128.0_F))

  frame: getContentPane(): add(label)
  frame: pack()
  frame: setVisible(true)
}
