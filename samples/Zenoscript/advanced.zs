// Advanced Zenoscript example

struct ApiResponse<T> {
  status: number;
  data: T;
  error?: string;
}

trait Processable<T, U> {
  process(input: T): U;
}

function fetchData<T>(url: string): ApiResponse<T> {
  // Simulating API call
  let response = {
    status: 200,
    data: null as T,
    error: null
  }
  
  if url == "" {
    response.status = 400
    response.error = "URL cannot be empty"
  }
  
  response
}

function processResponse<T>(response: ApiResponse<T>) {
  match response.status {
    200 => {
      console.log "Success"
      response.data
    }
    400 => {
      console.log "Bad request: " + response.error
      null
    }
    _ => {
      console.log "Unknown status: " + response.status
      null
    }
  }
}

let url = "https://api.example.com/data"
url |> fetchData |> processResponse |> console.log
