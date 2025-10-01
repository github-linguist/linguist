/*
 * Copyright 2024 Matthew Lutze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

mod Json.Parse {
    use Json.JsonElement.{JsonObject, JsonArray, JsonString, JsonNumber, JsonBool, JsonNull}
    use Json.JsonElement;
    use Option.flatMap;

    // Each main rule must start with a key character and consume trailing whitespace

    type alias ParseMonad[t] = Option[(t, List[Char])]


    ///
    /// Parses the given string into a JSON element.
    /// If the string does not represent a JSON element, None is returned.
    ///
    pub def parse(obj: String): Option[JsonElement] = {
        let in = String.toList(obj);
        forM (
            (_, tail) <- whitespace(in);
            (o, tail1) <- value(tail);
            _ <- eoi(tail1)
        ) yield {
            o
        }
    }

    def eoi(in: List[Char]): ParseMonad[Unit] = match in {
        case Nil => Some((), Nil)
        case _ :: _ => None
    }

    def whitespace(in: List[Char]): ParseMonad[Unit] = match in {
        case ' ' :: tail => whitespace(tail)
        case '\t' :: tail => whitespace(tail)
        case '\r' :: tail => whitespace(tail)
        case '\n' :: tail => whitespace(tail)
        case _ => Some((), in)
    }

    def object_(in: List[Char]): ParseMonad[JsonElement] = {
        forM (
            (_, tail) <- require('{', in);
            (_, tail1) <- whitespace(tail);
            (keyVals, tail2) <- keyVals(tail1);
            (_, tail3) <- require('}', tail2);
            (_, tail4) <- whitespace(tail3)
        ) yield {
            let map = List.toMap(keyVals);
            (JsonObject(map), tail4)
        }
    }

    def keyVals(in: List[Char]): ParseMonad[List[(String, JsonElement)]] = match in {
        // Case 1: starting a keyval pair
        case '\"' :: _ =>
            forM (
                (key, tail) <- string(in);
                (_, tail1) <- whitespace(tail);
                (_, tail2) <- require(':', tail1);
                (_, tail3) <- whitespace(tail2);
                (val, tail4) <- value(tail3);
                (_, tail5) <- whitespace(tail4);
                res <- match tail5 {
                    case ',' :: tail6 => forM (
                            (_, tail7) <- whitespace(tail6);
                            (tailKeyVals, tail8) <- keyVals(tail7)
                        ) yield {
                            let list = (key, val) :: tailKeyVals;
                            (list, tail8)
                        }
                    case '}' :: _ =>
                        let list = (key, val) :: Nil;
                        Some(list, tail5)
                    case _ => None
                }
            ) yield {
                res
            }

        // Case 2: end of list
        case '}' :: _ => Some(Nil, in)

        // Case 3: anything else is an error
        case _ => None
    }

    def array(in: List[Char]): ParseMonad[JsonElement] = {
        forM (
            (_, tail) <- require('[', in);
            (_, tail1) <- whitespace(tail);
            (elems, tail2) <- elements(tail1);
            (_, tail3) <- require(']', tail2)
        ) yield {
            (JsonArray(elems), tail3)
        }
    }

    def elements(in: List[Char]): ParseMonad[List[JsonElement]] = match in {
        case ']' :: _ => Some(Nil, in)
        case _ =>
            region rc {
                let lst = MutList.empty(rc);
                def walk(input) = {
                    let opt = forM(
                        (val, tail) <- value(input);
                        (_, tail1) <- whitespace(tail)
                    ) yield (val, tail1);
                    match opt {
                        case None => None
                        case Some(val, tail1) =>
                            MutList.push!(val, lst);
                            match tail1 {
                                case ']' :: _ => Some(tail1)
                                case ',' :: tail2 =>
                                    match whitespace(tail2) {
                                        case None => None
                                        case Some(_, tail3) => walk(tail3)
                                    }
                                case _ => None
                            }
                    }
                };
                forM(tl <- walk(in))
                    yield (MutList.toList(lst), tl)
            }
    }

    def value(in: List[Char]): ParseMonad[JsonElement] = match in {
        case '{' :: _ => object_(in)
        case '[' :: _ => array(in)
        case '\"' :: _ => stringValue(in)
        case 't' :: 'r' :: 'u' :: 'e' :: tail => Some(JsonBool(true), tail)
        case 'f' :: 'a' :: 'l' :: 's' :: 'e' :: tail => Some(JsonBool(false), tail)
        case 'n' :: 'u' :: 'l' :: 'l' :: tail => Some(JsonNull, tail)
        case _ => number(in)
    }

    def require(char: Char, in: List[Char]): ParseMonad[Unit] = match in {
        case head :: tail if char == head => Some((), tail)
        case _ => None
    }

    def stringValue(in: List[Char]): ParseMonad[JsonElement] = {
        forM (
            (str, tail) <- string(in)
        ) yield {
            (JsonString(str), tail)
        }
    }

    def string(in: List[Char]): ParseMonad[String] = {
        forM (
            (_, tail) <- require('\"', in);
            (contents, tail2) <- stringContents(tail);
            (_, tail3) <- require('\"', tail2)
        ) yield {
            let str = listToString(contents);
            (str, tail3)
        }
    }

    def listToString(list: List[Char]): String = region r {
            let sb = StringBuilder.empty(r);
            list |> List.forEach(c -> StringBuilder.append!(c, sb));
            StringBuilder.toString(sb)
    }

    def stringContents(in: List[Char]): ParseMonad[List[Char]] = stringContentsHelper(Nil, in)

    def stringContentsHelper(acc: List[Char], in: List[Char]): ParseMonad[List[Char]] = match in {
        case '\"' :: _ => Some(List.reverse(acc), in)

        case '\\' :: '\"' :: tail => stringContentsHelper('\"' :: acc, tail)

        case '\\' :: '\\' :: tail => stringContentsHelper('\\' :: acc, tail)

        case '\\' :: '/' :: tail => stringContentsHelper('/' :: acc, tail)

        case '\\' :: 'b' :: tail => stringContentsHelper('\u0008' :: acc, tail)

        case '\\' :: 'f' :: tail => stringContentsHelper('\u000C' :: acc, tail)

        case '\\' :: 'n' :: tail => stringContentsHelper('\n' :: acc, tail)

        case '\\' :: 'r' :: tail => stringContentsHelper('\r' :: acc, tail)

        case '\\' :: 't' :: tail => stringContentsHelper('\t' :: acc, tail)

        case '\\' :: 'u' :: c1 :: c2 :: c3 :: c4 :: tail =>
            let charRes = forM (
                string <- listToString(c1 :: c2 :: c3 :: c4 :: Nil) |> Some;
                intVal <- hexToInt(string)
            ) yield {
                import static java.lang.Character.toChars(Int32): Array[Char, Static] \ IO;
                unchecked_cast(toChars(intVal) |> Array.get(0) as _ \ {})
            };
            match charRes {
                case None => None
                case Some(char) => stringContentsHelper(char :: acc, tail)
            }

        // illegal escape
        case '\\' :: _ => None

        case c :: tail => stringContentsHelper(c :: acc, tail)

        // unexpected EOI
        case Nil => None
    }

    def hexToInt(hex: String): Option[Int32] = {
        import static java.lang.Integer.parseInt(String, Int32): Int32 \ IO;
        unchecked_cast(try {
            Some(parseInt(hex, 16))
        } catch {
            case _: ##java.lang.NumberFormatException => None
        } as _ \ {})
    }

    def number(in: List[Char]): ParseMonad[JsonElement] = match in {
        case c :: tail if isNumberChar(c) =>
            forM (
                (chars, tail1) <- numberTail(tail);
                string <- listToString(c :: chars) |> Some;
                bigDec <- BigDecimal.fromString(string)
            ) yield {
                let number = JsonNumber(bigDec);
                (number, tail1)
            }
        case _ => None
    }

    def isNumberChar(c: Char): Bool = Char.isDigit(c) or c == '+' or c == '-' or c == 'e' or c == 'E' or c == '.'

    def numberTail(in: List[Char]): ParseMonad[List[Char]] = match in {
        case c :: tail if isNumberChar(c) =>
            forM (
                (chars, tail1) <- numberTail(tail)
            ) yield {
                (c :: chars, tail1)
            }
        case _ => Some(Nil, in)
    }

}