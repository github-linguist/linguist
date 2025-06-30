// Pattern matching and pipe operators in Zenoscript

struct Container<T> {
  value: T;
  timestamp: number;
}

trait Processable<T> {
  process(): T;
}

// Using optional return and parentheses
function createContainer(value) {
  let timestamp = Date.now();
  { value, timestamp }
}

function processValue(container) {
  console.log "Processing:" container.value;
  container.value
}

let appState = :loading

let stateMessage = match appState {
  :idle => "Ready to start"
  :loading => "Please wait..."
  :success => "Operation completed"
  :error => "Failed permanently"
  _ => "Unknown state"
}

// Using simplified if with optional parentheses
if appState == :error {
  console.log "Error occurred!"
}

let data = createContainer "hello"
data |> processValue |> console.log
