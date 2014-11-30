module Sample

open System

type Foo =
    {
        Bar : string
    }

type Baz = interface end

let Sample1(xs : int list) : string =
    xs
    |> List.map (fun x -> string x)
    |> String.concat ","
