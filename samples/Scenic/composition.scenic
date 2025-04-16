"""A simple compositional scenario.

Adapted from the 'Composing Scenarios' tutorial in the Scenic documentation.
"""

param map = localPath('tests/formats/opendrive/maps/CARLA/Town01.xodr')
model scenic.simulators.newtonian.driving_model

scenario ParkedCar(gap=0.25):
    precondition: ego.laneGroup._shoulder != None
    setup:
        spot = OrientedPoint on visible ego.laneGroup.curb
        parkedCar = Car left of spot by gap

scenario Main():
    setup:
        ego = Car with behavior FollowLaneBehavior
        terminate after 10 seconds
    compose:
        while True:
            subScenario = ParkedCar(gap=0.25)
            do subScenario until (distance past subScenario.parkedCar) > 10
