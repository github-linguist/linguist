//===- ClasspathVMSystem/Properties.cpp -----------------------------------===//
//===--------------------- GNU classpath gnu/classpath/VMSystemProperties -===//
//
//                            The VMKit project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include <sys/utsname.h>

#include "types.h"

#include "Classpath.h"
#include "JavaArray.h"
#include "JavaClass.h"
#include "JavaObject.h"
#include "JavaThread.h"
#include "JavaUpcalls.h"
#include "Jnjvm.h"

#include "SetProperties.inc"

using namespace j3;

extern "C" {


JNIEXPORT void JNICALL Java_gnu_classpath_VMSystemProperties_preInit(
#ifdef NATIVE_JNI
JNIEnv *env,
jclass clazz,
#endif
JavaObject* prop) {
  
  llvm_gcroot(prop, 0);

  BEGIN_NATIVE_EXCEPTION(0)

  setProperties(prop);

  END_NATIVE_EXCEPTION
}

extern "C" void Java_gnu_classpath_VMSystemProperties_postInit__Ljava_util_Properties_2(JavaObject* prop) {

  llvm_gcroot(prop, 0);

  BEGIN_NATIVE_EXCEPTION(0)

  setCommandLineProperties(prop);

  END_NATIVE_EXCEPTION
}

}
