// Copyright (C) 2023 Florian Loitsch
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import openai

/**
If there is a gap of more than MAX_GAP between messages, we clear the
conversation.
*/
MAX_GAP ::= Duration --m=3
/** The maximum number of messages we keep in memory for each chat. */
MAX_MESSAGES ::= 20

class TimestampedMessage:
  text/string
  timestamp/Time
  is_from_assistant/bool

  constructor --.text --.timestamp --.is_from_assistant:

/**
A base class for a chat bot.

In addition to implementing the abstract methods $my_name_, $send_message_,
  subclasses must periodically call $clear_old_messages_. Ideally, this
  should happen whenever a new event is received from the server.

Typically, a `run` function proceeds in three steps:
```
run:
  while true:
    message := get_new_message  // From the chat server.
    clear_old_messages_         // Call to this bot.
    store_message_ message.text --chat_id=message.chat_id
    if should_respond:  // This might depend on the message or client.
      request_response_ message.chat_id
```

The chat_id is only necessary for bots that can be in multiple channels.
It's safe to use 0 if the bot doesn't need to keep track of multiple chats.

Once the bot receives a response it calls $send_message_.
*/
abstract class ChatBot:
  // The client is created lazily, to avoid memory pressure during startup.
  openai_client_/openai.Client? := null
  openai_key_/string? := ?

  // Maps from chat-id to deque.
  // Only authenticated chat-ids are in this map.
  all_messages_/Map := {:}

  /**
  Creates a new instance of the bot.

  The $max_gap parameter is used to determine if a chat has moved on to
    a new topic (which leads to a new conversation for the AI bot).

  The $max_messages parameter is used to determine how many messages
    are kept in memory for each chat.
  */
  constructor
      --openai_key/string
      --max_gap/Duration=MAX_GAP
      --max_messages/int=MAX_MESSAGES:
    openai_key_ = openai_key

  close:
    if openai_client_:
      openai_client_.close
      openai_client_ = null
      openai_key_ = null

  /** The name of the bot. Sent as a system message. */
  abstract my_name_ -> string

  /** Sends a message to the given $chat_id. */
  abstract send_message_ text/string --chat_id/any

  /** Returns the messages for the given $chat_id. */
  messages_for_ chat_id/any -> Deque:
    return all_messages_.get chat_id --init=: Deque

  /**
  Drops old messages from all watched chats.
  Uses the $MAX_GAP constant to determine if a chat has moved on to
    a new topic (which leads to a new conversation for the AI bot).
  */
  clear_old_messages_:
    now := Time.now
    all_messages_.do: | chat_id/any messages/Deque |
      if messages.is_empty: continue.do
      last_message := messages.last
      if (last_message.timestamp.to now) > MAX_GAP:
        print "Clearing $chat_id"
        messages.clear

  /**
  Builds an OpenAI conversation for the given $chat_id.

  Returns a list of $openai.ChatMessage objects.
  */
  build_conversation_ chat_id/any -> List:
    result := [
      openai.ChatMessage.system "You are contributing to chat of potentially multiple people. Your name is '$my_name_'. Be short.",
    ]
    messages := messages_for_ chat_id
    messages.do: | timestamped_message/TimestampedMessage |
      if timestamped_message.is_from_assistant:
        result.add (openai.ChatMessage.assistant timestamped_message.text)
      else:
        // We are not combining multiple messages from the user.
        // Typically, the chat is a back and forth between the user and
        // the assistant. For memory reasons we prefer to make individual
        // messages.
        result.add (openai.ChatMessage.user timestamped_message.text)
    return result

  /** Stores the $response that the assistant produced in the chat. */
  store_assistant_response_ response/string --chat_id/any:
    messages := messages_for_ chat_id
    messages.add (TimestampedMessage
      --text=response
      --timestamp=Time.now
      --is_from_assistant)

  /**
  Stores a user-provided $text in the list of messages for the
    given $chat_id.
  The $text should contain the name of the author.
  */
  store_message_ text/string --chat_id/any --timestamp/Time=Time.now -> none:
    messages := messages_for_ chat_id
    // Drop messages if we have too many of them.
    if messages.size >= MAX_MESSAGES:
        messages.remove_first

    new_timestamped_message := TimestampedMessage
        // We store the user with the message.
        // This is mainly so we don't need to create a new string
        // when we create the conversation.
        --text=text
        --timestamp=timestamp
        --is_from_assistant=false
    messages.add new_timestamped_message

  /**
  Sends a response to the given $chat_id.
  */
  send_response_ chat_id/any:
    if not openai_client_:
      if not openai_key_: throw "Closed"
      openai_client_ = openai.Client --key=openai_key_

    conversation := build_conversation_ chat_id
    response := openai_client_.complete_chat
        --conversation=conversation
        --max_tokens=300
    store_assistant_response_ response --chat_id=chat_id
    send_message_ response --chat_id=chat_id
