/// Tests for command interpreter functionality

/// Test for navigating to a location and retrieving an object
test GoToLocTest {
  functions [GenerateCommandListFineTuned]
  args {
    request "go to the living room then locate a knife and get it and bring it to Angel in the bathroom"
  }
  @@assert({{this.commands|length == 5}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "living room"}})
  @@assert({{this.commands[1].action == "pick_object" and this.commands[1].object_to_pick == "knife"}})
  @@assert({{this.commands[2].action == "go_to" and this.commands[2].location_to_go == "bathroom"}})
  @@assert({{this.commands[3].action == "find_person_by_name" and this.commands[3].name == "Angel"}})
  @@assert({{this.commands[4].action == "give_object"}})
}

/// Test for taking an object from a placement and delivering it
test TakeObjFromPlcmtTest {
  functions [GenerateCommandListFineTuned]
  args {
    request "take a pear from the sofa and deliver it to Charlie in the living room"
  }
  @@assert({{this.commands|length == 5}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "sofa"}})
  @@assert({{this.commands[1].action == "pick_object" and this.commands[1].object_to_pick == "pear"}})
  @@assert({{this.commands[2].action == "go_to" and this.commands[2].location_to_go == "living room"}})
  @@assert({{this.commands[3].action == "find_person_by_name" and this.commands[3].name == "Charlie"}})
  @@assert({{this.commands[4].action == "give_object"}})
}

/// Test for finding a person with specific gestures in a room
test FindPrsInRoomTest {
  functions [GenerateCommandList]
  args {
    request "locate a person pointing to the right in the bedroom and follow them to the dishwasher"
  }
  @@assert({{this.commands|length == 3}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "bedroom"}})
  @@assert({{this.commands[1].action == "find_person" and this.commands[1].attribute_value == "pointing to the right"}})
  @@assert({{this.commands[2].action == "follow_person_until" and this.commands[2].destination == "dishwasher"}})
}

/// Test for finding an object in a room and placing it elsewhere
test FindObjInRoomTest {
  functions [GenerateCommandList]
  args {
    request "look for a toy in the office then get it and put it on the kitchen table"
  }
  @@assert({{this.commands|length == 4}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "office"}})
  @@assert({{this.commands[1].action == "pick_object" and this.commands[1].object_to_pick == "toy"}})
  @@assert({{this.commands[2].action == "go_to" and this.commands[2].location_to_go == "kitchen"}})
  @@assert({{this.commands[3].action == "place_object"}})
}

/// Test for meeting a person at a location and answering questions
test MeetPrsAtBeacTest {
  functions [GenerateCommandList]
  args {
    request "meet Angel in the bathroom and answer a quiz"
  }
  @@assert({{this.commands|length == 3}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "bathroom"}})
  @@assert({{this.commands[1].action == "find_person_by_name" and this.commands[1].name == "Angel"}})
  @@assert({{this.commands[2].action == "answer_question"}})
}

/// Test for counting objects on a placement
test CountObjOnPlcmtTest {
  functions [GenerateCommandList]
  args {
    request "tell me how many foods there are on the bookshelf"
  }
  @@assert({{this.commands|length == 4}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "bookshelf"}})
  @@assert({{this.commands[1].action == "count" and this.commands[1].target_to_count == "foods"}})
  @@assert({{this.commands[2].action == "go_to" and this.commands[2].location_to_go == "start_location"}})
  @@assert({{this.commands[3].action == "say_with_context"}})
  @@assert({{ "how many foods there are on the bookshelf" in this.commands[3].user_instruction }})
}

/// Test for counting persons in a room with specific characteristics
test CountPrsInRoomTest {
  functions [GenerateCommandList]
  args {
    request "tell me how many persons pointing to the left are in the kitchen"
  }
  @@assert({{this.commands|length == 4}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "kitchen"}})
  @@assert({{this.commands[1].action == "count" and this.commands[1].target_to_count == "persons pointing to the left"}})
  @@assert({{this.commands[2].action == "go_to" and this.commands[2].location_to_go == "start_location"}})
  @@assert({{this.commands[3].action == "say_with_context"}})
  @@assert({{ "persons pointing to the left are in the kitchen" in this.commands[3].user_instruction }})
}

/// Test for retrieving information about a person at a location
test TellPrsInfoInLocTest {
  functions [GenerateCommandList]
  args {
    request "tell me the name of the person at the lamp"
  }
  @@assert({{this.commands|length == 5}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "lamp"}})
  @@assert({{this.commands[1].action == "find_person" and this.commands[1].attribute_value == ""}})
  @@assert({{this.commands[2].action == "get_person_info" and this.commands[2].info_type == "name"}})
  @@assert({{this.commands[3].action == "go_to" and this.commands[3].location_to_go == "start_location"}})
  @@assert({{this.commands[4].action == "say_with_context"}})
  @@assert({{ "name of the person at the lamp" in this.commands[4].user_instruction }})
}

/// Test for identifying object properties on a placement
test TellObjPropOnPlcmtTest {
  functions [GenerateCommandList]
  args {
    request "tell me what is the thinnest object on the kitchen table"
  }
  @@assert({{this.commands|length == 4}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "kitchen table"}})
  @@assert({{this.commands[1].action == "get_visual_info" and this.commands[1].object_to_get_info == "thinnest object"}})
  @@assert({{this.commands[2].action == "go_to" and this.commands[2].location_to_go == "start_location"}})
  @@assert({{this.commands[3].action == "say_with_context"}})
  @@assert({{ "thinnest object on the kitchen table" in this.commands[3].user_instruction }})
}

/// Test for conveying information to a person with specific gestures
test TalkInfoToGestPrsInRoomTest {
  functions [GenerateCommandList]
  args {
    request "say your team's affiliation to the waving person in the bathroom"
  }
  @@assert({{this.commands|length == 3}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "bathroom"}})
  @@assert({{this.commands[1].action == "find_person" and "waving" in this.commands[1].attribute_value}})
  @@assert({{this.commands[2].action == "say_with_context"}})
  @@assert({{ "team's affiliation" in this.commands[2].user_instruction }})
}

/// Test for answering questions from a gesturing person
test AnswerToGestPrsInRoomTest {
  functions [GenerateCommandList]
  args {
    request "answer the quiz of the person raising their left arm in the bedroom"
  }
  @@assert({{this.commands|length == 3}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "bedroom"}})
  @@assert({{this.commands[1].action == "find_person" and "raising their left arm" in this.commands[1].attribute_value}})
  @@assert({{this.commands[2].action == "answer_question" and this.commands[2].user_instruction is none}})
}

/// Test for following a named person from one location to another
test FollowNameFromBeacToRoomTest {
  functions [GenerateCommandList]
  args {
    request "follow Jules from the kitchen table to the bathroom"
  }
  @@assert({{this.commands|length == 3}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "kitchen table"}})
  @@assert({{this.commands[1].action == "find_person_by_name" and this.commands[1].name == "Jules"}})
  @@assert({{this.commands[2].action == "follow_person_until" and this.commands[2].location_to_follow == "bathroom"}})
}

/// Test for guiding a named person between locations
test GuideNameFromBeacToBeacTest {
  functions [GenerateCommandList]
  args {
    request "lead Robin from the side tables to the office"
  }
  @@assert({{this.commands|length == 3}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "side tables"}})
  @@assert({{this.commands[1].action == "find_person_by_name" and this.commands[1].name == "Robin"}})
  @@assert({{this.commands[2].action == "guide_person_to" and this.commands[2].location_to_guide == "office"}})
}

/// Test for guiding a person based on posture between locations
test GuidePrsFromBeacToBeacTest {
  functions [GenerateCommandList]
  args {
    request "take the lying person from the kitchen table to the desk lamp"
  }
  @@assert({{this.commands|length == 3}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "kitchen table"}})
  @@assert({{this.commands[1].action == "find_person" and "lying" in this.commands[1].attribute_value}})
  @@assert({{this.commands[2].action == "guide_person_to" and this.commands[2].location_to_guide == "desk lamp"}})
}

/// Test for guiding a person based on clothing between locations
test GuideClothPrsFromBeacToBeacTest {
  functions [GenerateCommandList]
  args {
    request "take the person wearing a white t shirt from the entrance to the armchair"
  }
  @@assert({{this.commands|length == 3}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "entrance"}})
  @@assert({{this.commands[1].action == "find_person" and "white t shirt" in this.commands[1].attribute_value}})
  @@assert({{this.commands[2].action == "guide_person_to" and this.commands[2].location_to_guide == "armchair"}})
}

/// Test for bringing an object from a placement to the user
test BringMeObjFromPlcmtTest {
  functions [GenerateCommandList]
  args {
    request "give me a pear from the cabinet"
  }
  @@assert({{this.commands|length == 4}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "cabinet"}})
  @@assert({{this.commands[1].action == "pick_object" and this.commands[1].object_to_pick == "pear"}})
  @@assert({{this.commands[2].action == "go_to" and this.commands[2].location_to_go == "start_location"}})
  @@assert({{this.commands[3].action == "give_object"}})
}

/// Test for identifying properties of a specific category on a placement
test TellCatPropOnPlcmtTest {
  functions [GenerateCommandList]
  args {
    request "tell me what is the thinnest snack on the storage rack"
  }
  @@assert({{this.commands|length == 4}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "storage rack"}})
  @@assert({{this.commands[1].action == "get_visual_info" and this.commands[1].object_to_get_info == "thinnest snack"}})
  @@assert({{this.commands[2].action == "go_to" and this.commands[2].location_to_go == "start_location"}})
  @@assert({{this.commands[3].action == "say_with_context"}})
  @@assert({{ "thinnest snack on the storage rack" in this.commands[3].user_instruction }})
}

/// Test for greeting a person by clothing description and answering a question
test GreetClothDscInRmTest {
  functions [GenerateCommandList]
  args {
    request "greet the person wearing a white coat in the kitchen and answer a question"
  }
  @@assert({{this.commands|length == 3}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "kitchen"}})
  @@assert({{this.commands[1].action == "find_person" and "white coat" in this.commands[1].attribute_value}})
  @@assert({{this.commands[2].action == "answer_question"}})
}

/// Test for greeting a named person and following them
test GreetNameInRmTest {
  functions [GenerateCommandList]
  args {
    request "say hello to Axel in the office and follow them"
  }
  @@assert({{this.commands|length == 3}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "office"}})
  @@assert({{this.commands[1].action == "find_person_by_name" and this.commands[1].name == "Axel"}})
  @@assert({{this.commands[2].action == "follow_person_until" and this.commands[2].destination == "cancelled"}})
}

/// Test for meeting a person at one location and finding them in another
test MeetNameAtLocThenFindInRmTest {
  functions [GenerateCommandList]
  args {
    request "meet Axel at the coatrack then locate them in the bathroom"
  }
  @@assert({{this.commands|length == 4}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "coatrack"}})
  @@assert({{this.commands[1].action == "find_person_by_name" and this.commands[1].name == "Axel"}})
  @@assert({{this.commands[2].action == "go_to" and this.commands[2].location_to_go == "bathroom"}})
  @@assert({{this.commands[3].action == "find_person" and this.commands[3].attribute_value == "Axel"}})
}

/// Test for counting people with specific clothing in a room
test CountClothPrsInRoomTest {
  functions [GenerateCommandList]
  args {
    request "tell me how many people in the living room are wearing white shirts"
  }
  @@assert({{this.commands|length == 4}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "living room"}})
  @@assert({{this.commands[1].action == "count" and this.commands[1].target_to_count == "people wearing white shirts"}})
  @@assert({{this.commands[2].action == "go_to" and this.commands[2].location_to_go == "start_location"}})
  @@assert({{this.commands[3].action == "say_with_context"}})
  @@assert({{ "how many people in the living room are wearing white shirts" in this.commands[3].user_instruction }})
}

/// Test for conveying information about a person to another person
test TellPrsInfoAtLocToPrsAtLocTest {
  functions [GenerateCommandList]
  args {
    request "tell the pose of the person at the chairs to the person at the kitchen table"
  }
  @@assert({{this.commands|length == 6}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "chairs"}})
  @@assert({{this.commands[1].action == "find_person" and this.commands[1].attribute_value == ""}})
  @@assert({{this.commands[2].action == "get_person_info" and this.commands[2].info_type == "pose"}})
  @@assert({{this.commands[3].action == "go_to" and this.commands[3].location_to_go == "kitchen table"}})
  @@assert({{this.commands[4].action == "find_person" and this.commands[4].attribute_value == ""}})
  @@assert({{this.commands[5].action == "say_with_context"}})
  @@assert({{ "pose" in this.commands[5].user_instruction }})
}

/// Test for following a person based on posture at a location
test FollowPrsAtLocTest {
  functions [GenerateCommandList]
  args {
    request "follow the sitting person in the kitchen"
  }
  @@assert({{this.commands|length == 3}})
  @@assert({{this.commands[0].action == "go_to" and this.commands[0].location_to_go == "kitchen"}})
  @@assert({{this.commands[1].action == "find_person" and "sitting" in this.commands[1].attribute_value}})
  @@assert({{this.commands[2].action == "follow_person_until" and this.commands[2].destination == "cancelled"}})
}
