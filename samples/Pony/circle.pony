use "collections"

class Circle
  var _radius: F32

  new create(radius': F32) =>
    _radius = radius'

  fun ref get_radius(): F32 =>
    _radius

  fun ref get_area(): F32 =>
    F32.pi() * _radius.pow(2)

  fun ref get_circumference(): F32 =>
    2 * _radius * F32.pi()

actor Main
  new create(env: Env) =>

    for i in Range[F32](1.0, 101.0) do
      let c = Circle(i)

      var str =
        "Radius: " + c.get_radius().string() + "\n" +
        "Circumference: " + c.get_circumference().string() + "\n" +
        "Area: " + c.get_area().string() + "\n"

      env.out.print(str)
    end
