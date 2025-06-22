/// Service robot command interpreter using specific schemas per action
/// https://github.com/RoBorregos/frida-cortex

class GoTo {
  action "go_to"
  location_to_go "start_location" | string @description(#"The location to go to,
  (kitchen, living room table, etc.)
  if asked to return something to the initial user use 'start_location'"#)
  @@description(#"Use this command to go to a specific location which can be a
  room, furniture from a room, start_location, etc."#)
}

class PickObject {
  action "pick_object"
  object_to_pick string @description("Name of the object to pick")
  @@description(#"Use this command to pick a specific object"#)
}

class FindPersonByName {
  action "find_person_by_name"
  name string @description("Name of the person to find")
  @@description(#"Use this command to find a person by name"#)
}

class FindPerson {
  action "find_person"
  attribute_value "" | string @description(#"Can be a feature/pose (pointing to
  the left, standing, blue shirt, etc.). If just asked to find ANY person, leave empty"#)
  @@description(#"Use this command to find a person whose name is not known,
  and an optional attribute is provided to identify the person"#)
}

class Count {
  action "count"
  target_to_count string @description(#"Name of the object/person to count
  (snacks, persons pointing to the left, persons wearing blue shirt, etc.)"#)
  @@description(#"Use this command to count the number of objects/persons,
  identified by a characteristic or category"#)
}

class GetPersonInfo {
  action "get_person_info"
  info_type "pose"|"gesture"|"name"|string @description(#"Information to be
  retrieved about the person (pose, gesture, name, clothing, age, etc.)"#)
  @@description(#"Use this command to get information about a person,
  info type is the type of information to be retrieved.
  Can ONLY be used AFTER a person is found by the find_person action"#)
}

class GetVisualInfo {
  action "get_visual_info"
  measure string @description(#"The property which will be measured to find the
  desired object in the environment (color, shape, size, thinnest, biggest, etc.)"#)
  object_category string @description(#"The category of the object to be found
  (snack, drink, dish, object, etc.)"#)
  @@description(#"Use this command to get information about the visualized
  environment, specifying the type of object to look for and a measure to find
  the outstanding one, it usually follows a 'go_to' command"#)
}

class AnswerQuestion {
  action "answer_question"
  @@description(#"Use this command to trigger the action of having a conversation
  with the user in which questions will be asked and answered"#)
}

class FollowPersonUntil {
  action "follow_person_until"
  destination "cancelled"|string @description(#"The destination location to which
  the robot will follow the person until, can be a room, furniture, etc. or 
  'cancelled' if the robot is to follow the person until asked to stop"#)
  @@description(#"Use this command to follow a person until a specific
  destination or requested to stop"#)
}

class GuidePersonTo {
  action "guide_person_to"
  destination_room string @description(#"The destination to guide or lead a person to
  can be a room, furniture, etc."#)
  @@description(#"Use this command to guide a person to a specific destination"#)
}

class GiveObject {
  action "give_object"
  @@description(#"Use this command to give the currently held object to a person
  this object is already stored in the system's memory, just use the command to
  trigger the action. It usually follows a 'pick_object' and 'go_to' command"#)
}

class PlaceObject {
  action "place_object"
  @@description(#"Use this command to place the currently held object on the
  current location, the object and location are already stored in the system's memory,
  just use the command to trigger the action. It usually follows a 'pick_object'
  and 'go_to' command"#)
}

class SayWithContext {
  action "say_with_context"
  user_instruction string @description(#"Instruction that will help to system
  to return the desired information to the user, it can be a question or a
  statement like 'tell me how many foods there are on the bookshelf'"#)
  previous_command_info "introduction"|string @description(#"Previous command needed to return
  the desired information to the user, it has to be a previous command that
  have been executed ('count', 'get_visual_info', etc.), or
  internal information that the system has stored in its memory 
  ('time', 'affection', etc.)"#)
  @@description(#"Use this command to say a message with specific information
  obtained from previous commands or internal information. It's different to
  answer_question since here it just the robot talking without a conversation"#)
}

type AvailableCommands = GoTo | PickObject | FindPersonByName | FindPerson | Count | GetPersonInfo | GetVisualInfo | AnswerQuestion | FollowPersonUntil | GuidePersonTo | GiveObject | PlaceObject | SayWithContext

/// Robot command list model
class CommandListLLM {
  commands AvailableCommands[] @description("List of commands for the robot to execute")
}

/// System prompt for non-finetuned model
template_string RobotServiceSystem() #"

You are a service robot for domestic applications.
 Now you are located in a house environment and we will give you general purpose tasks in the form of natural language.
 
 You have in your architecture the modules of:
 - navigation
 - manipulation
 - person recognition
 - object detection
 - human-robot interaction
 
 Your job is to understand the task and divide it into smaller actions proper to your modules,
 considering a logical flow of the actions along the time. For example, for the prompt
 'Locate a dish in the kitchen then get it and give it to Angel in the living room', the result would be: 
         
 {
     "commands": [
         {'action': 'go_to', 'location_to_go': 'kitchen'},
         {'action': 'pick_object', 'object_to_pick': 'dish'},
         {'action': 'go_to', 'location_to_go': 'living room'},
         {'action': 'find_person_by_name', 'name': 'Angel'},
         {'action': 'give_object'}
     ]
 }
 
 Another important thing is that when we give you the general task, some complements are grouped in categories.
 For example: apple, banana and lemon are all of them in the fruits category; cola, milk and red wine are all of them in the drinks category.
 If we give you a task talking about an item category, do not change the category word. It is very important you don't make up information
 not given explicitly. If you add new words, we will be disqualified. 
 For example:  'navigate to the bedroom then locate a food'.
 
 Another important thing is that you have to rememeber the name of the person, in case we are talking about 
 someone specifically. An example for the prompt can be: 'Get a snack from the 
 side tables and deliver it to Adel in the bedroom'.

 The system will handle the instructions and collect the response for each command.
 You can set the commands to use information from previous commands and context, for example:
 'tell me how many foods there are on the bookshelf'
 {
  "commands": [
    {"action": "go_to", "location_to_go": "bookshelf"},
    {"action": "count", "target_to_count": "foods"},
    {"action": "go_to", "location_to_go": "start_location"},
    {"action": "say_with_context", "user_instruction": "how many foods there are on the bookshelf", "previous_command_info": "count"}
  ]
 }
 Note from this example that if a user request something to be told or given back to their,
 you should first 'go_to' the 'start_location' and then perform the action. This is always the case.
 Remember something being asked back to the original user needs a go_to command to the start_location.
 But if its not inferred that the robot should go back to the start_location, you should NOT use it:
 'find a person in the living room who is pointing to the right and state the day of the month'
 {
  "commands": [
    {"action": "go_to", "location_to_go": "living room"},
    {"action": "find_person","attribute_value": "person pointing to the right"},
    {"action": "say_with_context","user_instruction": "locate a person pointing to the right in the living room and say the day of the month","previous_command_info": "the day of the month"}
  ]
 }

 The action get_person_info, guide_person_to and follow_person_until can ONLY be used after a person is found by the find_person or find_person_by_name action.
 The actions place_object can only be used after a pick_object action and usually follows a go_to command.
 The action give_object can only be used after a pick_object action and usually follows a go_to action, if not asked to return to start_location, it usually follows a find_person or find_person_by_name command also.
 The action say_with_context can only be used after a previous command that has been executed.
 The action find_person_by_name and find_person usually follows a go_to command.
 The action count usually follows a go_to command.
 The action get_visual_info usually follows a go_to command.
 The action answer_question can only be used after a previous find_person or find_person_by_name command.

"#

function GenerateCommandListFineTuned(request: string) -> CommandListLLM {
  client LOCAL_FINETUNED
  prompt #"
    {{ _.role("system") }}
    You are a command interpreter for a robot. Your task is to interpret the user's command and convert it into a structured format that the robot can understand.

    {{ _.role("user") }}
    {{ request }}
  "#
}

function GenerateCommandList(request: string) -> CommandListLLM {
  client GEMINI_FLASH_2_5
  prompt #"
    {{ _.role("system") }}
    {{ RobotServiceSystem() }}
    Follow this schema for the response, addressing each element of the commands,
    return in JSON format:
    {{ ctx.output_format }}

    {{ _.role("user") }}
    Generate commands for this prompt in the specified format:
    {{ request }}
  "#
}


function GenerateEnrichedAndReorderedCommand(request: string) -> string {
  client GEMINI_FLASH_2_5
  prompt #"
    You will receive a natural language command to be executed by a robot.
    Your task is to both paraphrase AND reorder the command while keeping the exact same intent and task objectives.
    
    Steps to follow:
    1. Paraphrase the command using synonyms for actions while maintaining the same meaning
    2. Change the word order to create a different sentence structure
    3. Keep all the same information - do not add or remove any details
    
    For example, if the command is:
    'go to the living room then locate a knife and get it and bring it to Angel in the bathroom',
    the result could be:
    'Navigate toward Angel in the bathroom after proceeding to the living room to find and collect a knife'
    
    Another example:
    'Pick up the red cup from the kitchen counter'
    could become:
    'From the kitchen counter, collect the red cup'
    
    Just return one transformed command in one line.
    {{ request }}
  "#
}

test GenerateEnrichedAndReorderedCommandTest {
  functions [GenerateEnrichedAndReorderedCommand]
  args {
    request "look for a toy in the office then get it and put it on the kitchen table"
  }
}