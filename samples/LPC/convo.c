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
 * Stops the current conversation.
 * This will also fire on_convoFinished
 * @param reason - the reason the convo was stopped
 */
public varargs stop(int reason) {
  debug(sprintf("stop request convo %d reason %d", convoId, reason));
  
  if (onFinishOb) {
    onFinishOb->on_convoFinished(convoId, reason);
  }

  queue_status=0;   
  while(remove_call_out("exec_next_op") != -1);
  
  if (destructOnEnd) {
    destruct(TO);
  }

  return 1;
}

/********** CONVO OPS ***************/

/** 
 * Random Op
 * execute a random op from a sub-convo 
 * @param subConvo - the convo object to execute a random op from
 */
private execOpRandom(object subConvo) {
  debug(sprintf("random op %d", convoId));
  if (queue_status==1) {
    mixed *ops = subConvo->query_queue();      
    int opIdx = RANDBETWEEN(0,sizeof(ops));
    subConvo->execOp(ops[opIdx]);  
  } else {    
    debug(sprintf("convo stopped, aborting random op %d", convoId));
    subConvo->stop();
  }
}

/**
 * Repeat Op
 * starts the convo over at the beginning, 
 * but does not erase the queue
 */
execOpRepeat() {  
  debug(sprintf("repeat convo %d", convoId));
  if (queue_status==1) {
    lastIdx=0;
    queue_status = 0;
    start_internal();
  } else {
    debug(sprintf("convo was stopped, skipping repeat %d", convoId));
  }
}

/**
 * Executes a conversation op
 * @param op - the operation to execute
 */
public int execOp(mixed op) {  
  if (!owner) {
    debug(sprintf("convo %d has no owner, aborting current op", convoId));
    stop(STOP_REASON_ABORTED);
    return 0;
  } else if (parentConvo && !parentConvo->query_isRunning()) {
    debug(sprintf("convo %d parent has stopped, aborting current op", convoId));
    stop(STOP_REASON_ABORTED);
    return 0;
  }
  
  object room = ENV(owner); // get current room of monster
  
  if (op["closure"]) {     
    apply(op["closure"], room);
  } else if (op["repeat"]) {
    execOpRepeat();
    return 0;
  } else if (op["delay"]) {            
    lastIdx++;  
    call_out("exec_next_op",op["delay"]);
    return 0;
  } else if (op["random"]) {
    execOpRandom(op["random"]);
  } else if (op["waitfor"]) {        
    // start timeout if there is one
    while (remove_call_out("waitfor_ttlExpired") != -1);        
    int ttl = op["waitfor"]["matchset"]->query_ttl();
    if (ttl) {      
      call_out("waitfor_ttlExpired",ttl);
    }

    // if there is a promptset, then start it
    object ps = op["waitfor"]["promptset"];    
    if (objectp(ps)) {
      debug("got a promptset");
      ps->start_internal();
      currentPromptSet = ps;
    }

    return 0; // exit here. catch_tell will advance index
  }  

  // check to make sure the op didn't stop the queue
  if (queue_status) {
    return 1;
  } else {
    debug(sprintf("Convo %d was stopped by op", convoId));
    if (destructOnEnd) {
      destruct(TO);
    }
  }

  return 0;
}

/**
 * Executes the next op into the convo queue
 */
exec_next_op() {
  debug(sprintf("convo=%d, exec next %d",convoId, lastIdx));

  // exit if there is nothing left to do
  if (!queue || lastIdx >= sizeof(queue)) {
    if (lastIdx >= sizeof(queue)) {
      // convo has finished
      // notify any objects that are waiting
      stop();
    }
    
    queue_status=0;
    lastIdx=0;
    queue = ({}); // cleanup
    return;
  }

  // get the next op and execute it
  mixed op = queue[lastIdx];
  int execNext = execOp(op);
  
  // execute the next op if appropriate
  if (execNext) {
    lastIdx++;
    exec_next_op();
  }
}

public start_internal() {  
  
  debug(sprintf("convo %d start. queue size is %d, status is %d",convoId, sizeof(queue), queue_status));
  if (queue_status) return 0;

  queue_status=1;
  call_out("exec_next_op", 0);
  
  return queue_status;

}

/**
 * Start the queue
 */
public start() {
  return start_internal();
}

public try_auto_start() {
  if (auto_start && !queue_status) {
    debug(sprintf("convo %d autostarted", convoId));
    return start_internal();
  }

  return 0;
}

/**
 * this will stop execution of the existing conversation
 * and clear any queued operations
 */
public restart() {      
  destructConvos();

  queue_status = 0;
  queue = ({});
  lastIdx = 0;
  while(remove_call_out("exec_next_op") != -1);
  
  debug(sprintf("convo %d restarted", convoId));

  return 1;
}

/************ OP CREATION METHODS **************/

/**
 * Speak operation
 * Will display a message in the owner's current environment.
 * The message will be prefixed with: [Short Name] says:
 * @param s - the message to display
 * @param to - an optional player name or object to speak to
 */
public varargs speak(string s, mixed to) {
  if (to) {
    object player;
    if (stringp(to)) {
      player = find_living(to);
    } else if (objectp(to) && living(to)) {
      player = to;
    } else {
      throw("to was not a string or object");
    }    
    
    closure op = (:
      if (ENV(player) == $1) {        
        string nm = owner->query_short();
        string pNm = player->query_name();
        string pfxO = sprintf("%s says [to you]: ", nm);
        string pfxR = sprintf("%s says [to %s]: ", nm, pNm);
        tell_object(to, sprintf("%s%s\n", pfxO, s), strlen(pfxO));
        tell_room(
          $1,
          sprintf("%s%s\n", pfxR, s),
          ({ player, owner }),
          strlen(pfxR)
        );
      }
      return 1;
    :);
    queue += ({ ([ "closure": op ]) });
  } else {  
    string pfx = sprintf("%s says: ", owner->query_short());    
    string msg = sprintf("%s%s\n", pfx, s);

    closure op = (: tell_room($1, msg, ({ owner })), strlen(pfx) :);
    queue += ({ ([ "closure": op ]) });
  }

  try_auto_start();  
  return TO;  
}

/**
 * Delay operation
 * Pauses execution for t seconds
 */
public delay(int t) {
  queue += ({ ([ "delay": t ]) });
  try_auto_start();
  return TO;
}

/**
 * Tell operation
 * Will display message "s" to player "player" via a tell mechanism
 * The message will be prefixed with: Short Name tells you:
 */
public tell(string player, string s) {  
  string msg = sprintf("%s tells you: %s\n", owner->query_short(), s);

  closure op = (: object p=find_player(player); if(p) { tell_object(p, msg); } return; :);
  queue += ({ ([ "closure": op ]) });
  
  try_auto_start();
  return TO;
}

/**
 * Emote operation
 * The message e will be displayed in the owner's current environment.
 * The message will be prefixed with: Short Name.
 * Eg:
 *  ->emote("giggles")
 * Output:
 *  Scarry Monster giggles
 */
public emote(string e) {
  object room = ENV(owner);
  string msg = sprintf("%s %s\n", owner->query_short(), e);
  
  closure op = (: tell_room($1, msg, ({ owner })) :);
  queue += ({ ([ "closure": op ]) });
  
  try_auto_start();
  return TO;  
}

/**
 * Wait For Operation
 * This will pause execution of the current convo queue until either
 * a message has been received that matches one of the conditions in the 
 * matchset, or until the matchsets timeout has elapsed.
 * 
 * An optional promptset can be provided which is a convo queue that will
 * execute while the owner is waiting for an appropriate response.
 *
 * Note: For the wait_for operation to work correctly, the monster must pipe
 * it's catch_tell specifically to the convo object.
 * Example:
 * if ("convo"::catch_tell(str)) {
 *   return 1;
 * } else {     
 *   return ::catch_tell(str);
 * } 
 */
public varargs wait_for(object _matchset, object _promptset) {
  debug(sprintf("waitfor types: %d and %d", typeof(_matchset), typeof(_promptset)));
  object *wf = ([ "matchset": _matchset ]);

  // TODO typecheck these objects
  if (objectp(_promptset)) {        
    wf["promptset"] = _promptset;    
  } 
  
  queue += ({ ([ "waitfor": wf ]) });

  try_auto_start();
  return TO;  
}

/** 
 * Matchset operation.
 * Creates a new matchset
 */
public matchset() {
  object m = clone_object(MATCHSET_OB);
  return m;
}

/**
 * Promptset operation
 * Creates a new promptset, which is really just another convo object
 */
public promptset() {
  debug("created prompset");
  // a promptset is just a convo
  return convo();  
}

/**
 * Stop All operation
 * Stops this convo and all upstream convo's
 */
public stopall() {
  object currentConvo = TO;
  closure op = (: TO->stop(STOP_REASON_STOPALL) :);
  queue += ({ ([ "closure": op ]) });
  
  return TO;
}

/**
 * Random operation
 * Accepts a convo object as a parameter and will execute a random op from
 * that sub-convo's queue. Note the ops in the sub-convo will not be executed
 * sequentially. Only a single op will be executed.
 */
public random(object convo) {  
  
  // TODO: typecheck the convo object    
  convo->set_parentConvo(TO);  
  queue += ({ ([ "random": convo ]) });
  
  return TO;
}

/**
 * Repeat operation
 * Causes the current convo queue to start over at the beginning. 
 * Make sure there is some sort of delay in your queue so that you don't
 * flood the user with output
 */
public repeat() {
  queue += ({ ([ "repeat": 1 ])});
  return TO;
}

/**
 * Convo operation
 * Creates a new conversation queue
 */
 convo() {
  object c = clone_object(CONVO_OB);
  c->set_owner(query_owner());
  c->set_parentConvo(TO);
  c->set_auto_start(0);  

  childConvos += ({ c });

  return c;
}

/**
 * Function operation
 * Will execute a function via call_other on the owner object.
 */
fn(string func, varargs mixed *args) {
  
  object currentConvo = TO;
  closure op = (: call_other(owner, func, currentConvo, args...) :);
  queue += ({ ([ "closure": op ]) });
  try_auto_start();
  return TO;  

}

/**
 * Execute a sub operation used by the wait_for mechanics
 */
execute_sub_op(mixed op) {
  // what are we executing?
  if (objectp(op)) debug(sprintf("exec subop type: %s", load_name(op)));

  if (objectp(op) && load_name(op)==CONVO_OB) {
    debug("exec subop convo");
    // branch convo, execute and wait for response
    waitForConvoToFinish = op->set_onFinishOb(TO);
    op->start_internal();
    return 0; // exit here so the next op does not get executed  
  } else {
    debug("unhandled subop");
  }

  lastIdx++;
  exec_next_op();

  return 1;
}

/**
 * called when the wait_for's ttl has expired
 */
waitfor_ttlExpired() {
  catch_tell("***TTL_EXPIRED***");
}

/**
 * Stops the current promptset, if there is one
 */
stop_promptSet() {
  
  debug("Stopping promptset");

  if (currentPromptSet) {
    currentPromptSet->stop();    
    currentPromptSet = 0;
    
    debug("Promptset stopped");
  }

}

/**
 * Handle incoming messages
 */
catch_tell(string str) {  
  debug("incoming tell: " + str);

  if (!TP || !interactive(TP)) {
    debug("TP not interactive");
	  return;
  }

  if (!queue || lastIdx >= sizeof(queue)) {
    debug("queue not running");
    queue_status=0;
    return;
  }

  // peak at the next op, if it a wait type and we're not waiting
  // for a branch convo to finish, then proceed
  object op = queue[lastIdx];    
  if (op && mappingp(op) && op["waitfor"]) {    
    object matches = op["waitfor"]["matchset"];    
    
    // handle TTL expiration
    if (matches->query_ttl() && str=="***TTL_EXPIRED***") {
      debug("handling ttl expired");
      while (remove_call_out("waitfor_ttlExpired") != -1);
      stop_promptSet();
      execute_sub_op(matches->query_ttl_op());      
    }

    // only check if we're not waiting for a convo
    if (!waitForConvoToFinish) {      
      foreach(mixed *m in matches->query_matchset()) {
        string pat  = m[0];
        mixed resp  = m[1];
        if (sizeof(regexp(({str}), pat)) > 0) {        
          set_lastPlayer(TP); // store the player that interacted here
          while (remove_call_out("waitfor_ttlExpired") != -1);
          stop_promptSet();
          execute_sub_op(resp);          
          return;
        }
      }

      debug("no responses matched");
    }
  }    
}

/**
 * Handles the on_convoFinished fired from sub-convos (usually in the context)
 * of a wait_for's promptset
 */
on_convoFinished(int convoId, int reason) {
  debug(sprintf("onfinished %d reason %d",convoId, reason));
  if (waitForConvoToFinish==convoId) {
    waitForConvoToFinish = 0;
    if (reason == STOP_REASON_STOPALL) {
      stop(STOP_REASON_STOPALL);
    } else {
      lastIdx++;
      exec_next_op();
    }
  }
}

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