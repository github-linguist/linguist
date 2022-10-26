"""
TITLE: Bypassing 05
AUTHOR: Francis Indaheng, findaheng@berkeley.edu
DESCRIPTION: Ego vehicle performs multiple lane changes to bypass three 
slow adversary vehicles.
SOURCE: NHSTA, #16
"""

#################################
# MAP AND MODEL                 #
#################################

param map = localPath('../../../../tests/formats/opendrive/maps/CARLA/Town03.xodr')
param carla_map = 'Town03'
model scenic.simulators.carla.model

#################################
# CONSTANTS                     #
#################################

MODEL = 'vehicle.lincoln.mkz2017'

param EGO_SPEED = VerifaiRange(7, 10)
param EGO_BRAKE = VerifaiRange(0.5, 1.0)

param ADV1_DIST = VerifaiRange(20, 25)
param ADV2_DIST = globalParameters.ADV1_DIST + VerifaiRange(15, 20)
param ADV3_DIST = globalParameters.ADV2_DIST + VerifaiRange(15, 20)
param ADV_SPEED = VerifaiRange(2, 4)

BYPASS_DIST = 15
INIT_DIST = 50
TERM_DIST = globalParameters.ADV3_DIST + 15

#################################
# AGENT BEHAVIORS               #
#################################

behavior EgoBehavior():
	try:
		do FollowLaneBehavior(target_speed=globalParameters.EGO_SPEED)
	interrupt when ((distance to adversary_1) < BYPASS_DIST
				 or (distance to adversary_3) < BYPASS_DIST):
		newLaneSec = self.laneSection.laneToRight
		do LaneChangeBehavior(
			laneSectionToSwitch=newLaneSec,
			target_speed=globalParameters.EGO_SPEED)
	interrupt when (distance to adversary_2) < BYPASS_DIST:
		newLaneSec = self.laneSection.laneToLeft
		do LaneChangeBehavior(
			laneSectionToSwitch=newLaneSec,
			target_speed=globalParameters.EGO_SPEED)

behavior Adversary2Behavior():
	rightLaneSec = self.laneSection.laneToRight
	do LaneChangeBehavior(
		laneSectionToSwitch=rightLaneSec,
		target_speed=globalParameters.ADV_SPEED)
	do FollowLaneBehavior(target_speed=globalParameters.ADV_SPEED)

#################################
# SPATIAL RELATIONS             #
#################################

initLane = Uniform(*filter(lambda lane:
	all([sec._laneToRight is not None for sec in lane.sections]),
	network.lanes))
egoSpawnPt = OrientedPoint in initLane.centerline
egoLaneSecToSwitch = initLane.sectionAt(egoSpawnPt).laneToRight

#################################
# SCENARIO SPECIFICATION        #
#################################

adversary_1, adversary_2, adversary_3 = Car, Car, Car

ego = Car at egoSpawnPt,
	with blueprint MODEL,
	with behavior EgoBehavior()

adversary_1 = Car following roadDirection for globalParameters.ADV1_DIST,
	with blueprint MODEL,
	with behavior FollowLaneBehavior(target_speed=globalParameters.ADV_SPEED)

adversary_2 = Car following roadDirection for globalParameters.ADV2_DIST,
	with blueprint MODEL,
	with behavior Adversary2Behavior()

adversary_3 = Car following roadDirection for globalParameters.ADV3_DIST,
	with blueprint MODEL,
	with behavior FollowLaneBehavior(target_speed=globalParameters.ADV_SPEED)

require (distance to intersection) > INIT_DIST
require (distance from adversary_1 to intersection) > INIT_DIST
require (distance from adversary_2 to intersection) > INIT_DIST
require (distance from adversary_3 to intersection) > INIT_DIST
terminate when (distance to adversary_3) > TERM_DIST
