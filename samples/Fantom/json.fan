using util

class Json
{
  public static Void main ()
  {
    Str input := """{"blue": [1, 2], "ocean": "water"}"""
    Map jsonObj := JsonInStream(input.in).readJson

    echo ("Value for 'blue' is: " + jsonObj["blue"])
    jsonObj["ocean"] = ["water":["cold", "blue"]]
    Map ocean := jsonObj["ocean"]
    echo ("Value for 'ocean/water' is: " + ocean["water"])
    output := JsonOutStream(Env.cur.out)
    output.writeJson(jsonObj)
    echo ()
  }
}
