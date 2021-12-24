// Taken from https://github.com/gleam-experiments/snag
// Apache-2.0 licensed
import gleam/string_builder
import gleam/string
import gleam/list
import gleam/int

/// A Snag is a boilerplate-free error type that can be used to track why an
/// error happened, though does not store as much detail on specific errors as a
/// custom error type would.
///
/// It is useful in code where it must either pass or fail, and when it fails we
/// want good debugging information to print to the user. i.e. Command line
/// tools, data processing pipelines, etc.
///
/// If it not suited to code where the application needs to make a decision about
/// what to do in the event of an error, such as whether to give up or to try
/// again. i.e. Libraries, web application backends, API clients, etc.
/// In these situations it is recommended to create a custom type for your errors
/// as it can be pattern matched on and have any additional detail added as
/// fields.
pub type Snag {
  Snag(issue: String, cause: List(String))
}

/// A concise alias for a `Result` that uses a `Snag` as the error value.
pub type Result(t) =
  Result(t, Snag)

/// Create a new `Snag` with the given issue text.
///
/// See also the `error` function for creating a `Snag` wrapped in a `Result`.
///
/// # Example
///
/// ```gleam
/// > new("Not enough credit")
/// > |> line_print
/// "error: Not enough credit"
/// ```
pub fn new(issue: String) -> Snag {
  Snag(issue: issue, cause: [])
}

/// Create a new `Snag` wrapped in a `Result` with the given issue text.
///
/// # Example
///
/// ```gleam
/// > error("Not enough credit")
/// Error(new("Not enough credit"))
/// ```
pub fn error(issue: String) -> Result(success) {
  Error(new(issue))
}

/// Add additional contextual information to a `Snag`.
///
/// See also the `context` function for adding contextual information to a `Snag`
/// wrapped in a `Result`.
///
/// # Example
///
/// ```gleam
/// > new("Not enough credit")
/// > |> layer("Unable to make purchase")
/// > |> line_print
/// "error: Unable to make purchase <- Not enough credit"
/// ```
pub fn layer(snag: Snag, issue: String) -> Snag {
  Snag(issue: issue, cause: [snag.issue, ..snag.cause])
}

/// Add additional contextual information to a `Snag` wrapped in a `Result`.
///
/// # Example
///
/// ```gleam
/// > error("Not enough credit")
/// > |> context("Unable to make purchase")
/// > |> result.map_error(line_print)
/// Error("error: Unable to make purchase <- Not enough credit")
/// ```
pub fn context(result: Result(success), issue: String) -> Result(success) {
  case result {
    Ok(_) -> result
    Error(snag) -> Error(layer(snag, issue))
  }
}

/// Turn a snag into a multi-line string, optimised for readability.
///
/// # Example
///
/// ```gleam
/// > new("Not enough credit")
/// > |> layer("Unable to make purchase")
/// > |> layer("Character creation failed")
/// > |> pretty_print
/// "error: Character creation failed
///
/// cause:
///   0: Unable to make purchase
///   1: Not enough credit
/// "
/// ```
pub fn pretty_print(snag: Snag) -> String {
  let builder = string_builder.from_strings(["error: ", snag.issue, "\n"])

  string_builder.to_string(case snag.cause {
    [] -> builder
    cause ->
      builder
      |> string_builder.append("\ncause:\n")
      |> string_builder.append_builder(pretty_print_cause(cause))
  })
}

fn pretty_print_cause(cause) {
  cause
  |> list.index_map(fn(index, line) {
    string.concat(["  ", int.to_string(index), ": ", line, "\n"])
  })
  |> string_builder.from_strings
}

/// Turn a snag into a single-line string, optimised for compactness. This may be
/// useful for logging snags.
///
/// # Example
///
/// ```gleam
/// > new("Not enough credit")
/// > |> layer("Unable to make purchase")
/// > |> layer("Character creation failed")
/// > |> pretty_print
/// "error: Character creation failed <- Unable to make purchase <- Not enough credit"
/// ```
pub fn line_print(snag: Snag) -> String {
  [string.append("error: ", snag.issue), ..snag.cause]
  |> string.join(" <- ")
}
