# Copyright 2012-2014 Institut National des Sciences Appliquées de Lyon (INSA-Lyon)
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

module samples.PrepostDecorator

import gololang.Decorators

let isInteger = isOfType(Integer.class)

@checkResult(isString(): andThen(lengthIs(2)))
@checkArguments(isInteger: andThen(isPositive()), isString())
function foo = |a, b| {
    return b + a
}

let myCheck = checkArguments(isInteger: andThen(isPositive()))

@myCheck
function inv = |v| -> 1.0 / v

let isPositiveInt = isInteger: andThen(isPositive())

@checkArguments(isPositiveInt)
function mul = |v| -> 10 * v

@checkArguments(isNumber())
function num = |v| -> "ok"

@checkArguments(isNotNull())
function notnull = |v| -> "ok"

function main = |args| {
    try { println(foo(1, "b")) } catch (e) { println(e) }
    try { println(foo(-1, "b")) } catch (e) { println(e) }
    try { println(foo("a", 2)) } catch (e) { println(e) }
    try { println(foo(1, 2)) } catch (e) { println(e) }
    try { println(foo(10, "ab")) } catch (e) { println(e) }

    try { println(inv(10)) } catch (e) { println(e) }
    try { println(inv(0)) } catch (e) { println(e) }

    try { println(mul(5)) } catch (e) { println(e) }
    try { println(mul(0)) } catch (e) { println(e) }

    try { println(num(1)) } catch (e) { println(e) }
    try { println(num(1_L)) } catch (e) { println(e) }
    try { println(num(1.5)) } catch (e) { println(e) }
    try { println(num(1.5_F)) } catch (e) { println(e) }
    try { println(num("a")) } catch (e) { println(e) }
    try { println(num('a')) } catch (e) { println(e) }

    try { println(notnull('1')) } catch (e) { println(e) }
    try { println(notnull(null)) } catch (e) { println(e) }
}
