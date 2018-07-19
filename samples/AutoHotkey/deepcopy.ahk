DeepCopy(Array, Objs=0)
{
    If !Objs
        Objs := Object()
    Obj := Array.Clone() ; produces a shallow copy in that any sub-objects are not cloned
    Objs[&Array] := Obj ; Save this new array - & returns the address of Array in memory
    For Key, Val in Obj
        If (IsObject(Val)) ; If it is a subarray
            Obj[Key] := Objs[&Val] ; If we already know of a reference to this array
            ? Objs[&Val] ; Then point it to the new array (to prevent infinite recursion on self-references
            : DeepCopy(Val,Objs) ; Otherwise, clone this sub-array
    Return Obj
}
