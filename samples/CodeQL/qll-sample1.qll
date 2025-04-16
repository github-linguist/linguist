/**
 * Source: https://github.com/github/rasmuswl-temp-ql-clone/blob/5e1c8fe8c9c3505c35f7e032169bf56d71ea2bc7/cpp/ql/src/semmle/code/cpp/models/implementations/Inet.qll
 * License: Apache License 2.0
 */

import semmle.code.cpp.models.interfaces.Taint
import semmle.code.cpp.models.interfaces.ArrayFunction

class InetNtoa extends TaintFunction {
  InetNtoa() { hasGlobalName("inet_ntoa") }

  override predicate hasTaintFlow(FunctionInput input, FunctionOutput output) {
    input.isParameter(0) and
    output.isReturnValueDeref()
  }
}

class InetAton extends TaintFunction, ArrayFunction {
  InetAton() { hasGlobalName("inet_aton") }

  override predicate hasTaintFlow(FunctionInput input, FunctionOutput output) {
    input.isParameterDeref(0) and
    output.isParameterDeref(1)
  }

  override predicate hasArrayInput(int bufParam) { bufParam = 0 }

  override predicate hasArrayOutput(int bufParam) { bufParam = 1 }

  override predicate hasArrayWithNullTerminator(int bufParam) { bufParam = 0 }

  override predicate hasArrayWithFixedSize(int bufParam, int elemCount) {
    bufParam = 1 and
    elemCount = 1
  }
}
