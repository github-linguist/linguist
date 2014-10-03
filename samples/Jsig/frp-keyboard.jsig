import { Observ } from "observ"
import { Delegator } from "dom-delegator"

type KeyCode : Number
type Direction : "left" | "right" | "up" | "down" | "void"
type Coord : {
    x: Number,
    y: Number,
    lastPressed: Direction
}

type NativeKeyboard : {
    isDown: (keyCode: KeyCode) => Observ<Boolean>,
    keysDown: Observ<Array<keyCode: KeyCode>>,
    keyDown: Observ<keyCode: KeyCode>,
    lastPressed: Observ<keyCode: KeyCode>,
    directions: (
        up: KeyCode, down: KeyCode, left: KeyCode, right: KeyCode
    ) => Observ<Coord>
}

type Keyboard : NativeKeyboard & {
    arrows: Observ<Coord>,
    wasd: Observ<Coord>,
    ctrl: Observ<Boolean>,
    shift: Observ<Boolean>
}

frp-keyboard : () => cachedKeyboard: Keyboard

frp-keyboard/keyboard : (Delegator) => Keyboard

frp-keyboard/native : (Delegator) => NativeKeyboard
