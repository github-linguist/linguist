/**
 * SOURCE: https://github.com/Aelto/tw3-shared-utils
 * LICENSE: https://github.com/Aelto/tw3-shared-utils/blob/master/LICENSE
 */


/**
 * This is the abstract class for the event listener. If you want to add an event
 * listener to an NPC in order to run your custom code, create a new class that
 * extends `SU_InteractionEventListener` and override the `run` method.
 */
abstract class SU_InteractionEventListener {

  /**
   * The tag is not used by the code, but it could be useful in case you want to
   * verify if an NPC already has an event listener with a specific tag.
   * It's your only way to identify the event listeners without doing a full
   * bitwise comparison.
   */
  public var tag: string;
  default tag = "None";

  /**
   * This method is run whenever the NPC had his interaction event triggered.
   * You get as parameters the actionName as well as the activator, those two
   * parameters are the default parameters the CNewNPC::OnInteraction event has
   * and then this method has a third parameter that is the NPC the player
   * interacted with.
   */
  public function run(actionName : string, activator : CEntity, receptor: CPeristentEntity): bool {
    /**
     * The return value here dictactes if the CNewNPC::OnInteraction event should
     * continue after your code or if it should end.
     * A return value of `false` will tell the event to stop there and not go
     * any further.
     */
    return true;
  }

}

function SU_NpcInteraction_runAllInteractionListeners(actionName: string, activator: CEntity, receptor: CEntity): bool {
  var current_event_listener: SU_InteractionEventListener;
  var persistent_entity: CPeristentEntity;
  var should_event_continue: bool;
  var i: int;

  should_event_continue = true;

  if ((CPeristentEntity)receptor) {
    persistent_entity = (CPeristentEntity)receptor;

    for (i = 0; i < persistent_entity.onInteractionEventListeners.Size(); i += 1) {
      current_event_listener = persistent_entity.onInteractionEventListeners[i];

      should_event_continue = should_event_continue && current_event_listener.run(
        actionName,
        activator,
        persistent_entity
      );
    }
  }


  return should_event_continue;
}

function SU_NpcInteraction_hasEventListenerWithTag(npc: CPeristentEntity, tag: string): bool {
  var current_event_listener: SU_InteractionEventListener;
  var i: int;

  for (i = 0; i < npc.onInteractionEventListeners.Size(); i += 1) {
    current_event_listener = npc.onInteractionEventListeners[i];
    
    if (current_event_listener.tag == tag) {
      return true;
    }
  }

  return false;
}

function SU_removeInteractionEventListenerByTag(npc: CPeristentEntity, tag: string) {
  var current_event_listener: SU_InteractionEventListener;
  var i: int;
  
  for (i = 0; i < npc.onInteractionEventListeners.Size(); i += 1) {
    current_event_listener = npc.onInteractionEventListeners[i];

    if (current_event_listener.tag != tag) {
      continue;
    }

    if (i == npc.onInteractionEventListeners.Size() - 1) {
      npc.onInteractionEventListeners.PopBack();
      continue;
    }

    npc.onInteractionEventListeners.Erase(i);
    i -= 1;
  }
}
