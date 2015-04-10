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

module samples.SwingActionListener

import java.awt.event
import javax.swing
import javax.swing.WindowConstants

local function listener = |handler| -> asInterfaceInstance(ActionListener.class, handler)

function main = |args| {

  let frame = JFrame("Action listeners")
  frame: setDefaultCloseOperation(EXIT_ON_CLOSE())

  let button = JButton("Click me!")
  button: setFont(button: getFont(): deriveFont(96.0_F))

  # Using a helper function
  button: addActionListener(listener(|event| -> println("Clicked!")))

  # Using a standard augmentation: MethodHandle::to(Class)
  button: addActionListener((|event| -> println("[click]")): to(ActionListener.class))

  # Straight closure passing
  button: addActionListener(|event| -> println("( )"))

  frame: getContentPane(): add(button)
  frame: pack()
  frame: setVisible(true)
}
