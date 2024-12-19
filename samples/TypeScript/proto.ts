// Code generated by protoc-gen-ts_proto. DO NOT EDIT.
// versions:
//   protoc-gen-ts_proto  v1.181.2
//   protoc               v5.28.2
// source: hello.proto

/* eslint-disable */
import _m0 from "protobufjs/minimal";
import { Any } from "./google/protobuf/any";

export const protobufPackage = "helloworld";

export interface HelloWorld {
  payload: Any | undefined;
}

function createBaseHelloWorld(): HelloWorld {
  return { payload: undefined };
}

export const HelloWorld = {
  encode(
    message: HelloWorld,
    writer: _m0.Writer = _m0.Writer.create()
  ): _m0.Writer {
    if (message.payload !== undefined) {
      Any.encode(message.payload, writer.uint32(10).fork()).ldelim();
    }
    return writer;
  },

  decode(input: _m0.Reader | Uint8Array, length?: number): HelloWorld {
    const reader =
      input instanceof _m0.Reader ? input : _m0.Reader.create(input);
    let end = length === undefined ? reader.len : reader.pos + length;
    const message = createBaseHelloWorld();
    while (reader.pos < end) {
      const tag = reader.uint32();
      switch (tag >>> 3) {
        case 1:
          if (tag !== 10) {
            break;
          }

          message.payload = Any.decode(reader, reader.uint32());
          continue;
      }
      if ((tag & 7) === 4 || tag === 0) {
        break;
      }
      reader.skipType(tag & 7);
    }
    return message;
  },

  fromJSON(object: any): HelloWorld {
    return {
      payload: isSet(object.payload) ? Any.fromJSON(object.payload) : undefined,
    };
  },

  toJSON(message: HelloWorld): unknown {
    const obj: any = {};
    if (message.payload !== undefined) {
      obj.payload = Any.toJSON(message.payload);
    }
    return obj;
  },

  create<I extends Exact<DeepPartial<HelloWorld>, I>>(base?: I): HelloWorld {
    return HelloWorld.fromPartial(base ?? ({} as any));
  },
  fromPartial<I extends Exact<DeepPartial<HelloWorld>, I>>(
    object: I
  ): HelloWorld {
    const message = createBaseHelloWorld();
    message.payload =
      object.payload !== undefined && object.payload !== null
        ? Any.fromPartial(object.payload)
        : undefined;
    return message;
  },
};

type Builtin =
  | Date
  | Function
  | Uint8Array
  | string
  | number
  | boolean
  | undefined;

export type DeepPartial<T> = T extends Builtin
  ? T
  : T extends globalThis.Array<infer U>
  ? globalThis.Array<DeepPartial<U>>
  : T extends ReadonlyArray<infer U>
  ? ReadonlyArray<DeepPartial<U>>
  : T extends {}
  ? { [K in keyof T]?: DeepPartial<T[K]> }
  : Partial<T>;

type KeysOfUnion<T> = T extends T ? keyof T : never;
export type Exact<P, I extends P> = P extends Builtin
  ? P
  : P & { [K in keyof P]: Exact<P[K], I[K]> } & {
      [K in Exclude<keyof I, KeysOfUnion<P>>]: never;
    };

function isSet(value: any): boolean {
  return value !== null && value !== undefined;
}
