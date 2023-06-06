#pragma once

#ifndef SOME_TEST
[["java:package:linguist"]]
#endif

module Linguist
{
    enum MyEnum
    {
        One,
        Two,
        Three
    }

    struct MyStruct
    {
        // An int
        int a;
        /* string */
        string b;

        MyEnum e;
    }

    exception MyException {
        string e;
    }

    dictionary<string, string> MyDict;

    sequence<MyEnum> MyEnumSeq;

    class BaseClass {
        int value = -1;
    }

    class MyClass extends BaseClass
    {
        MyDict info;

        optional(1) string op;
    }


    interface MyInterface
    {
        void operationA(out bool valid);
        idempotent void operationB(int a);
        MyEnumseq getEnum();

        ["cpp:const", "cpp:noexcept"] string getName();
    }
}
