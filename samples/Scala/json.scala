scala> import scala.util.parsing.json.{JSON, JSONObject}
import scala.util.parsing.json.{JSON, JSONObject}

scala> JSON.parseFull("""{"foo": "bar"}""")
res0: Option[Any] = Some(Map(foo -> bar))

scala> JSONObject(Map("foo" -> "bar")).toString()
res1: String = {"foo" : "bar"}
