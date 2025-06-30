// Basic Zenoscript example

struct User {
  name: string;
  age: number;
  active: boolean;
}

trait Serializable {
  serialize(): string;
}

function greet(user) {
  let message = "Hello, " + user.name
  console.log message
  message
}

let user = {
  name: "John",
  age: 30,
  active: true
}

let status = :active

match status {
  :active => console.log "User is active"
  :inactive => console.log "User is inactive"
  _ => console.log "Unknown status"
}

user |> greet |> console.log
