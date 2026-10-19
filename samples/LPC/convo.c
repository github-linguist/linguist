/*
 * LDMud mob dialog system
 * Copyright 2022-2024 John Chmura
 *
 */
#include "../../jgambit.h"
#include "./convo.h"

private object owner;
private object parentConvo;
private object *childConvos = ({});
private int auto_start = 1;
private int queue_status = 0;
private mixed *queue = ({});
private int lastIdx=0;
private int convoId = random(10000)+1;
private int destructOnEnd = 0;

private object currentPromptSet;
/** the last player to interact with this convo */
private object lastPlayer;
private int waitForConvoToFinish = 0;

private object onFinishOb;

/* function declarations */
public int execOp(mixed op);


/* debug output */
private void debug(string s) {    
  if (CONVO_DEBUG) {
    object j = find_player("jgambit");
    if (j) {
      tell_object(j,COLORSTRING(sprintf("$PF[debug] %s\n", s), j));
    }
  }
}

/** returns 1 if the convo queue is running, otherwise 0*/
public query_isRunning() {
  return queue_status;
}

public set_lastPlayer(object p) {
  if (!living(p)) {
    debug("Tried to set last player but wasn't living");
    return 0;
  }

  debug(sprintf("Set last player to %s", p->query_name()));
  lastPlayer = p;  
  return p;
}
public query_lastPlayer() { 
  return (!lastPlayer && parentConvo)
            ? parentConvo->query_lastPlayer() 
            : lastPlayer; 
}

/** indicates if the convo should destruct when it is finished */
public set_destructOnEnd(int flag) {
  destructOnEnd = flag;
}

set_parentConvo(object c) {
  parentConvo=c;
}
object query_parentConvo() { return parentConvo; }

/** The owner is the actor this convo corresponds to. Usually a monster. */
set_owner(object o) {
  owner = o;
  return o;
}
object query_owner() { return owner; }

/** the operation queue for this conversation. don't mess with this */
set_queue(mixed *q) {
  queue = q;
  return q;
}
query_queue() { return queue; }

/** 
  * sets the autostart flag.
  * the default (1) will cause the convo to autostart when an operation is added
  * otherwise you will have to manually call convo->start()
  */
set_auto_start(int s) {
  auto_start = s;
  return 1;
}
int query_auto_start() { return auto_start; }

/** 
 * (Optional) 
 * Sets the object in which the "on_convoFinished(convoId)" function
 * function will be called when the conversation ends.
 * @param ob - object or string
 */
set_onFinishOb(mixed ob) {
  if (objectp(ob))
    onFinishOb = object_name(ob);
  else
    onFinishOb = ob;
  return convoId;
}
query_onFinishOb() { return onFinishOb; }

query_convoId() { return convoId; }

/**
 * does some cleanup by destructing all child convos
 */
public destructConvos() {
  foreach(object c in childConvos) {
    if (c) c->destructConvos();    
    destruct(c);
  }

  childConvos -= ({ 0 });
}
