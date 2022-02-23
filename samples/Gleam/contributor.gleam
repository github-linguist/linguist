import gleam/result
import gleam/option.{Option}
import gleam/dynamic.{DecodeError, Dynamic}
import gleam_contributors/json

pub type Contributor {
  Contributor(name: String, github: Option(String))
}

// Could include avatarURl and websiteUrl if required
pub type Contributorspage {
  Contributorspage(
    nextpage_cursor: Result(String, Nil),
    contributor_list: List(Contributor),
  )
}

// This is still parsing the response json into Gleam types, see
// parse_contributors, but it is the contributor section only. To make the parse
// function more readable
pub fn decode(json_obj: Dynamic) -> Result(Contributor, DecodeError) {
  try author = dynamic.field(json_obj, "author")

  try dynamic_name = dynamic.field(author, "name")
  try name = dynamic.string(dynamic_name)
  let github = {
    try user = dynamic.field(author, "user")
    try dynamic_github = dynamic.field(user, "url")
    dynamic.string(dynamic_github)
  }
  Ok(Contributor(name: name, github: option.from_result(github)))
}

/// Converts response json into Gleam type. Represents one page of contributors
pub fn decode_page(
  response_json: String,
) -> Result(Contributorspage, DecodeError) {
  let res = json.decode(response_json)

  try data = dynamic.field(res, "data")
  try repo = dynamic.field(data, "repository")
  try object = dynamic.field(repo, "object")
  try history = dynamic.field(object, "history")
  try pageinfo = dynamic.field(history, "pageInfo")
  try dynamic_nextpage = dynamic.field(pageinfo, "hasNextPage")
  try nextpage = dynamic.bool(dynamic_nextpage)
  let cursor = case nextpage {
    False -> Error(Nil)
    True ->
      dynamic.field(pageinfo, "endCursor")
      |> result.then(dynamic.string)
      |> result.map_error(fn(_) { Nil })
  }
  try nodes = dynamic.field(history, "nodes")
  try contributors = dynamic.typed_list(nodes, of: decode)
  Ok(Contributorspage(nextpage_cursor: cursor, contributor_list: contributors))
}
