# Chatathon protocol definition

- Transfer encoding is 7-bit ASCII text. Messages MUST NOT contain any bytes with the most significant bit set.
- Each message between client and server is delimited by a \n character (0x0a)
- Each message starts with a "message type" in all caps, then additional arguments separated by spaces
- Messages the server may send to the client:
  - `OK` `<message>`. When a client's message has been handled successfully, the server MUST respond with an `OK` message, and MAY include a message to further describe the success. These optional messages SHOULD NOT be shown to the user.
  - `ERROR` `<reason>`, MUST be sent whenever a client's request cannot be fulfilled or understood, and SHOULD include a string describing the cause of the problem.
  - `MESSAGE` `<room>` `<sender>` `<content>`. When a client is in a chat room and someone else in that room sends a chat message to it, the server MUST notify each connected client. The `content` of a message may contain additional space (`0x20`) characters.
  - `WHISPER` `<sender>` `<content>`. When a client receives a private message from another client, the server MUST send a `WHISPER`. As with `MESSAGE`, the `content` may include space characters.
  - `JOIN` `<room>` `<user>`. When a client is in a chat room and another user enters the room, the server SHOULD send a `JOIN` notification to the client.
  - `LEAVE` `<room>` `<user>`. When a client is in a chat room and another user leaves the room or disconnects from the server, the server SHOULD send a `LEAVE` notification to the client.

The server MUST NOT "echo" messages from a client back to itself: a client should never see a `MESSAGE`, `JOIN`, or `LEAVE` message originating from its own username; the server MAY send `WHISPER` messages from a client to itself, or MAY ignore them.

If a client disconnects from the server without first sending a `LOGOUT`, the server MUST behave as if the client had first sent the `LOGOUT`. A server MUST NOT send any messages to a client after `OK`ing a `LOGOUT`.

- Messages the client may send to the server:
  - `LOGIN` `<name>`
    - MUST be the first message sent when a client connects
    - Sets a name to identify the client by
    - `name` MUST NOT include space characters (`0x20`)
    - If the requested name is already taken, the server MUST respond with an ERROR message, and MUST either immediately sever the connection or wait for another `LOGIN` message
    - Client MUST NOT send another LOGIN message after logging in.
  - `LOGOUT`
    - When the user wishes to disconnect from the chat server, the client SHOULD send a `LOGOUT` message to the server before closing the connection. The client MUST NOT send any futher messages after sending the `LOGOUT` message.
  - `JOIN` `<room>`
    - Indicates that the user wishes to receive messages sent to the named room.
    - Users must be in a room in order to send `MESSAGE`s to it.
    - If the user is already in the named room, the server MUST respond with an `OK`, but MUST NOT send `JOIN` notifications to other users in the room.
    - The server MUST allow a user to be in multiple rooms at once, but a client MAY choose to only allow users to be in a single room.
  - `LEAVE` `<room>`
    - The user wishes to no longer be in the named room.
    - If any `MESSAGE`s are sent to the named room after the server has `OK`ed the `LEAVE` request, the client SHOULD NOT display them to the user.
  - `SAY` `<room>` `<message>`
    - Send a message to all users in the named room.
    - If the user is not in the named room, the server `MUST` respond with an `ERROR`, and `MUST NOT` send the `<message>` to any other clients.
  - `WHISPER` `<user>` `<message>`
    - Send a private message to the named user
    - If the named user is not logged in, the server SHOULD respond with an `ERROR`.

## Sample conversation between client and server

```text
> LOGIN akm
< ERROR name 'akm' is already taken
> LOGIN amalloy
< OK
> JOIN factual
< OK joined, now in 1 channels
> JOIN clojure
< OK joined, now in 2 channels
< MESSAGE factual acrow hey man welcome to the meetup!
> SAY factual hi, everyone
< OK sent message to factual
> LEAVE clojure
< OK left, now in 1 channel
< WHISPER acrow hope you got this chat server thing ready!
> WHISPER acrow yeah, gotta finish setting it up now.
< OK WHISPER
> LOGOUT
< OK bye
```
