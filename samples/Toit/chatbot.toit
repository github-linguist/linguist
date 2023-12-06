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
MAX-GAP ::= Duration --m=3
/** The maximum number of messages we keep in memory for each chat. */
MAX-MESSAGES ::= 20

class TimestampedMessage:
  text/string
  timestamp/Time
  is-from-assistant/bool

  constructor --.text --.timestamp --.is-from-assistant:

/**
A base class for a chat bot.

In addition to implementing the abstract methods $my-name_, $send-message_,
  subclasses must periodically call $clear-old-messages_. Ideally, this
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

Once the bot receives a response it calls $send-message_.
*/
abstract class ChatBot:
  // The client is created lazily, to avoid memory pressure during startup.
  openai-client_/openai.Client? := null
  openai-key_/string? := ?

  // Maps from chat-id to deque.
  // Only authenticated chat-ids are in this map.
  all-messages_/Map := {:}

  /**
  Creates a new instance of the bot.

  The $max-gap parameter is used to determine if a chat has moved on to
    a new topic (which leads to a new conversation for the AI bot).

  The $max-messages parameter is used to determine how many messages
    are kept in memory for each chat.
  */
  constructor
      --openai-key/string
      --max-gap/Duration=MAX-GAP
      --max-messages/int=MAX-MESSAGES:
    openai-key_ = openai-key

  close:
    if openai-client_:
      openai-client_.close
      openai-client_ = null
      openai-key_ = null

  /** The name of the bot. Sent as a system message. */
  abstract my-name_ -> string

  /** Sends a message to the given $chat-id. */
  abstract send-message_ text/string --chat-id/any

  /** Returns the messages for the given $chat-id. */
  messages-for_ chat-id/any -> Deque:
    return all-messages_.get chat-id --init=: Deque

  /**
  Drops old messages from all watched chats.
  Uses the $MAX-GAP constant to determine if a chat has moved on to
    a new topic (which leads to a new conversation for the AI bot).
  */
  clear-old-messages_:
    now := Time.now
    all-messages_.do: | chat-id/any messages/Deque |
      if messages.is-empty: continue.do
      last-message := messages.last
      if (last-message.timestamp.to now) > MAX-GAP:
        print "Clearing $chat-id"
        messages.clear

  /**
  Builds an OpenAI conversation for the given $chat-id.

  Returns a list of $openai.ChatMessage objects.
  */
  build-conversation_ chat-id/any -> List:
    result := [
      openai.ChatMessage.system "You are contributing to chat of potentially multiple people. Your name is '$my-name_'. Be short.",
    ]
    messages := messages-for_ chat-id
    messages.do: | timestamped-message/TimestampedMessage |
      if timestamped-message.is-from-assistant:
        result.add (openai.ChatMessage.assistant timestamped-message.text)
      else:
        // We are not combining multiple messages from the user.
        // Typically, the chat is a back and forth between the user and
        // the assistant. For memory reasons we prefer to make individual
        // messages.
        result.add (openai.ChatMessage.user timestamped-message.text)
    return result

  /** Stores the $response that the assistant produced in the chat. */
  store-assistant-response_ response/string --chat-id/any:
    messages := messages-for_ chat-id
    messages.add (TimestampedMessage
      --text=response
      --timestamp=Time.now
      --is-from-assistant)

  /**
  Stores a user-provided $text in the list of messages for the
    given $chat-id.
  The $text should contain the name of the author.
  */
  store-message_ text/string --chat-id/any --timestamp/Time=Time.now -> none:
    messages := messages-for_ chat-id
    // Drop messages if we have too many of them.
    if messages.size >= MAX-MESSAGES:
        messages.remove-first

    new-timestamped-message := TimestampedMessage
        // We store the user with the message.
        // This is mainly so we don't need to create a new string
        // when we create the conversation.
        --text=text
        --timestamp=timestamp
        --is-from-assistant=false
    messages.add new-timestamped-message

  /**
  Sends a response to the given $chat-id.
  */
  send-response_ chat-id/any:
    if not openai-client_:
      if not openai-key_: throw "Closed"
      openai-client_ = openai.Client --key=openai-key_

    conversation := build-conversation_ chat-id
    response := openai-client_.complete-chat
        --conversation=conversation
        --max-tokens=300
    store-assistant-response_ response --chat-id=chat-id
    send-message_ response --chat-id=chat-id
