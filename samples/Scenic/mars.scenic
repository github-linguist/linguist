"""Driving a rover through a debris field with a bottleneck.

This is the `examples/webots/mars/narrowGoal_dynamic.scenic` example
from the Scenic repository.
"""

# Dynamic version of the narrowGoal scenario.

model scenic.simulators.webots.mars.model

ego = Rover at 0 @ -2, with controller 'sojourner'

goal = Goal at Range(-2, 2) @ Range(2, 2.5)

terminate when (distance to goal) <= 0.2

monitor terminateOnT:
    keyboard = simulation().supervisor.getKeyboard()
    keyboard.enable(20)
    print('Select the 3D window and press T to terminate the scenario and generate a new one.')
    while True:
        wait
        if keyboard.getKey() == ord('T'):
            terminate

# Bottleneck made of two pipes with a rock in between

gap = 1.2 * ego.width
halfGap = gap / 2

bottleneck = OrientedPoint offset by Range(-1.5, 1.5) @ Range(0.5, 1.5), facing Range(-30, 30) deg

require abs((angle to goal) - (angle to bottleneck)) <= 10 deg

BigRock at bottleneck

leftEdge = OrientedPoint left of bottleneck by halfGap,
    facing Range(60, 120) deg relative to bottleneck.heading
rightEdge = OrientedPoint right of bottleneck by halfGap,
    facing Range(-120, -60) deg relative to bottleneck.heading

Pipe ahead of leftEdge, with length Range(1, 2)
Pipe ahead of rightEdge, with length Range(1, 2)

# Other junk because why not?

Pipe
BigRock beyond bottleneck by Range(-0.5, 0.5) @ Range(0.5, 1)
BigRock beyond bottleneck by Range(-0.5, 0.5) @ Range(0.5, 1)
Rock
Rock
Rock
