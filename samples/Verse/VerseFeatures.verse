using. /Fortnite.com/Characters
using. /Fortnite.com/Devices
using. /UnrealEngine.com/Temporary
using. /Verse.org/Random
using. /Verse.org/Simulation

using. Wrappers

VerseFeatures<public> := module {

    ### Runtime Utils

    SleepTicks<public>(Count:int)<suspends>:void = for (X := 1..Count). Sleep(0.0)

    ### Logic Utils

    Nothing<public>()<computes>:void = void
    Success<public>()<computes><decides>:void = void
    Failure<public>()<computes><decides>:false = false? and Err("Unreachable")

    # Unsafe function call, it expect to always succeed, or else will throw a runtime error.
    #
    # It was made for usage on places where the result is already known to always succeed, such
    # as defining constants and avoiding patterns like `Something[] or Err("Unreachable")`
    Must<public>(Function:type{_(:t)<decides><transacts>:t2}, Args:t where t:type, t2:type)<transacts>:t2 = {
        Function[Args] or Err("This is your fault.")
    }
    Must<public>(Result:result(t, any) where t:type)<computes>:t = {
        Result.GetSuccess[] or Err("This is your fault.")
    }

    (VerseFeatures:)ToString<public>(Input:logic)<computes>:string = {
        if (Input?) { "true" } else { "false" }
    }

    CoinFlip<public>()<transacts>:logic = logic{GetRandomInt(0, 1) = 1}
    
    (Logic:logic).Invert<public>()<computes>:logic = logic{not Logic?}

    prefix'-'<public>(Logic:logic)<computes>:logic = Logic.Invert()



    ### Agent Utils

    IsRealPlayer<public>(Player:agent)<computes><decides>:player = player[Player]

    (Agent:agent).IsValid<public>()<decides><transacts>:void = {
        player[Agent].IsActive[] or not player[Agent]
        Agent.GetFortCharacter[].IsActive[]
    }

    (Agent:agent).AwaitValidation<public>(?MaxTimeout:float=Inf, ?RefreshInterval:float=0.1)<suspends>:logic = {
        CallbackEvent := event(agent){}
        branch {
            loop {
                if (Agent.IsValid[]). CallbackEvent.Signal(Agent)
                Sleep(Max(0.0, RefreshInterval))
            }
        }

        CallbackEvent.AwaitFor(Agent, ?MaxTimeout := MaxTimeout)
    }



    ### Message Utils
    
    JoinMessages<public><localizes>(First:message, Second:message)<computes>:message = "{First}{Second}"

    operator'+'<public><localizes>(First:message, Second:message)<computes>:message = "{First}{Second}"



    ### Map Operations
    
    # Returns an array with all keys of the map
    (Input:[k]v where k:subtype(comparable), v:type).Keys<public>()<computes>:[]k = {
        for (CurrentKey->_Unused : Input). CurrentKey
    }

    # Returns an array with all values of the map
    (Input:[k]v where k:subtype(comparable), v:type).Values<public>()<computes>:[]v = {
        for (CurrentValue : Input). CurrentValue
    }

    # Merges an array of maps into a single map.
    # In case of key conflicts, later maps override earlier ones.
    ReduceMaps<public>(InputMaps:[][k]v where k:subtype(comparable), v:type)<computes>:[k]v = {
        InputMapsLength := InputMaps.Length

        FirstMap := InputMaps[0] or (return map{})
        SecondMap := InputMaps[1] or (return FirstMap)

        MergedMap := ConcatenateMaps(FirstMap, SecondMap)

        OtherMaps := InputMaps.Slice[2] or Err("Unreachable")

        if (OtherMaps.Length = 0). return MergedMap

        return ReduceMaps(array{MergedMap} + OtherMaps)
    }

    # Returns a map without the map element that Key matches the Key provided
    # If Strict is set to true, will force a fail when key does not exist on the map
    (Input:[k]v).RemoveByKey<public>(KeyToRemove:k, (?Strict:logic=false) where k:subtype(comparable), v:type)<decides><transacts>:[k]v = {
        if (Strict?). Input[KeyToRemove]

        var TempMap:[k]v = map{}
        for (
            CurrentKey->CurrentValue:Input
            CurrentKey <> KeyToRemove
        ). set TempMap[CurrentKey] = CurrentValue
        TempMap
    }

    # Returns a map without all map elements that Key matches the Keys provided
    (Input:[k]v).RemoveByKeys<public>(KeysToRemove:[]k where k:subtype(comparable), v:type)<decides><transacts>:[k]v = {
        var TempMap:[k]v = Input
        for (KeyToRemove : KeysToRemove) {
            ResultMap := TempMap.RemoveByKey[KeyToRemove, ?Strict := false] # Strict default value is already false on the function definition and due to that is not a required argument, but if not mentioned again here, for some reason it crashes the server
            set TempMap = ResultMap
        }
        TempMap
    }

    # Returns a map with only the elements that Key matches the Keys provided
    (Input:[k]v).FilterByKeys<public>(Keys:[]k where k:subtype(comparable), v:type)<decides><transacts>:[k]v = {
        var TempMap:[k]v = map{}
        for (
            CurrentKey->CurrentValue:Input
            KeyToCompare : Keys
            CurrentKey = KeyToCompare
        ). set TempMap[CurrentKey] = CurrentValue
        TempMap
    }

    # Returns the first map Key where the associated Value matches the provided Value
    (Input:[k]v).FindKeyByValue<public>(Value:v where k:subtype(comparable), v:subtype(comparable))<decides><transacts>:k = {
        var MaybeKey:?k = false
        for (
            CurrentKey->CurrentValue:Input
            not MaybeKey?
            CurrentValue = Value
        ). set MaybeKey = option{CurrentKey}
        MaybeKey?
    }

    # Returns all map Keys where the associated Value matches the provided Value
    # If Strict is set to true, will force a fail when none of the values exist on the map
    (Input:[k]v).FindKeysByValue<public>(Value:v, (?Strict:logic=false) where k:subtype(comparable), v:subtype(comparable))<computes><decides>:[]k = {
        KeysFound := for (
            CurrentKey->CurrentValue:Input
            CurrentValue = Value
        ). CurrentKey

        KeysFound.Length > 0 or not Strict?

        KeysFound
    }

    # Return a map with only the elements that Value matches the Values provided
    # If Strict is set to true, will force a fail when none of the values exist on the map
    (Input:[k]v).FilterByValues<public>(Values:[]v, (?Strict:logic=false) where k:subtype(comparable), v:subtype(comparable))<decides><transacts>:[k]v = {
        var TempMap:[k]v = map{}
        for (
            CurrentKey->CurrentValue:Input
            ValueToCompare : Values
            CurrentValue = ValueToCompare
        ). set TempMap[CurrentKey] = CurrentValue

        TempMap.Length > 0 or not Strict?

        TempMap
    }

    # Returns the first map Key where the Value succeeds the provided condition function
    (Input:[k]v).FindKeyByValueData<public>(Condition:type{_(:v)<decides><transacts>:void} where k:subtype(comparable), v:type)<decides><transacts>:k = {
        var MaybeKey:?k = false
        for (
            CurrentKey->CurrentValue:Input
            not MaybeKey?
            Condition[CurrentValue]
        ). set MaybeKey = option{CurrentKey}
        MaybeKey?
    }

    # Returns all map Keys where the Value succeeds the provided condition function
    # If Strict is set to true, will force a fail when no compatible values exist on the map
    (Input:[k]v).FindKeysByValueData<public>(Condition:type{_(:v)<decides><transacts>:void}<#, (?Strict:logic=false)#> where k:subtype(comparable), v:type)<decides><transacts>:[]k = {
        KeysFound := for (
            CurrentKey->CurrentValue:Input
            Condition[CurrentValue]
        ). CurrentKey
        
        <#KeysFound.Length > 0 or not Strict?#>

        KeysFound
    }



    ### Array Operations

    <#> NOTE: Some functions bellow are written to be easy to understand, not necessarily optimized for performance.
        Many array operations makes copies of the entire array even when is not needed, and some iterations does
        not have early breaks/exists, making it less efficient due to always iterating over all array values.

    (Input:[]t where t:type).Reverse<public>()<computes>:[]t = {
        LastIndex := Input.Length - 1
        for (
            CurrentIndex->_Unused : Input
            Element := Input[LastIndex - CurrentIndex]
        ). Element
    }

    NormalizeIndex<public>(Length:int, Index:int)<computes><decides>:int = {
        Length >= 1

        NormalizedRange := Mod[Index, Length]

        NormalizedIndex := if (NormalizedRange < 0). Length + NormalizedRange
        else. NormalizedRange
    }

    (Input:[]t where t:type).At<public>(Index:int)<computes><decides>:t = {
        Input[NormalizeIndex[Input.Length, Index]]
    }

    (Input:[]t where t:subtype(comparable)).FindLast<public>(ElementToFind:t)<decides><transacts>:int = {
        LastIndex := Input.Length - 1

        var MaybeIndex:?int = false
        for (
            Index := 0..LastIndex
            not MaybeIndex?
            InverseIndex := LastIndex - Index
            Input[InverseIndex] = ElementToFind
        ). set MaybeIndex = option{InverseIndex}

        MaybeIndex?
    }

    (Input:generator(t) where t:type).AsArray<public>()<computes>:[]t = {
        for (Element : Input). Element
    }

    (Input:generator(t) where t:type).GetFirst<public>()<computes><decides>:t = {
        Input.AsArray()[0]
    }

    # Return the Index of the first Element on the array that succeeds the provided condition function
    (Input:[]t).FindBy<public>(Condition:type{_(:t)<decides><transacts>:void} where t:type)<decides><transacts>:int = {
        var MaybeIndex:?int = false
        for (
            Index->Element:Input
            not MaybeIndex?
            Condition[Element]
        ). set MaybeIndex = option{Index}
        MaybeIndex?
    }

    # Return the Index of the first Element on the array that succeeds the provided condition function with additional arguments
    (Input:[]t).FindByDynamic<public>(Condition:type{_(:t, :t2)<decides><transacts>:void}, ConditionArgs:t2 where t:type, t2:type)<decides><transacts>:int = {
        var MaybeIndex:?int = false
        for (
            Index->Element:Input
            not MaybeIndex?
            Condition[Element, ConditionArgs]
        ). set MaybeIndex = option{Index}
        MaybeIndex?
    }

    # Return an array with only the Elements that succeeds the provided condition function
    # If Strict is set to true, will force a fail when no compatible elements exist on the array
    (Input:[]t).FilterBy<public>(Condition:type{_(:t)<decides><transacts>:void}, (?Strict:logic=false) where t:type)<decides><transacts>:[]t = {
        NewArray := for (
            CurrentValue:Input
            Condition[CurrentValue]
        ). CurrentValue

        NewArray.Length > 0 or not Strict?

        NewArray
    }

    # Return an array with only the Elements that succeeds the provided condition function with additional arguments
    # If Strict is set to true, will force a fail when no compatible elements exist on the array
    (Input:[]t).FilterByDynamic<public>(Condition:type{_(:t, :t2)<decides><transacts>:void}, ConditionArgs:t2, (?Strict:logic=false) where t:type, t2:type)<decides><transacts>:[]t = {
        NewArray := for (
            CurrentValue:Input
            Condition[CurrentValue, ConditionArgs]
        ). CurrentValue

        NewArray.Length > 0 or not Strict?

        NewArray
    }



    ### Primitive Math Sorting Operations

    SortIntAscending<public>(Left:int, Right:int)<computes><decides>:void = {
        Left < Right
    }

    SortIntDescending<public>(Left:int, Right:int)<computes><decides>:void = {
        not SortIntAscending[Left, Right]
    }

    SortFloatAscending<public>(Left:float, Right:float)<computes><decides>:void = {
        Left < Right
    }

    SortFloatDescending<public>(Left:float, Right:float)<computes><decides>:void = {
        not SortFloatAscending[Left, Right]
    }



    ### Containers

    # Generic container class that can be used for FIFO/LIFO/Linked Lists and other common types of array data storage implementations
    <#> NOTE: due to current verse limitations, we can't have mutable members (variables) in parametric classes, and for
        that reason we need to handle and keep track of the container state changes outside of this class during usage.
    Container<public>(t:type) := class<computes> {
        Elements<public> :[]t = array{}

        PointerPosition<public>:int = 0

        Size<public>()<computes>:int = Elements.Length
        IsEmpty<public>()<computes><decides>:void = Elements.Length = 0
        IsPointerValid<public>(?Position:int = PointerPosition)<computes><decides>:void = Position >= 0 and Position < Elements.Length # Should only fail if has 0 elements due to the class automatically sanitizing the pointer position

        SetPointerPosition<public>(Position:int, ?Circular:logic = false)<computes><decides>:Container(t) = Container(t){
            PointerPosition := if (Circular?) {
                NormalizeIndex[Elements.Length, Position]
            } else {
                IsPointerValid[?Position := Position]
                Position
            }
            Elements := Elements
        }
        SetPointerOffset<public>(Offset:int, ?Circular:logic = false)<computes><decides>:Container(t) = SetPointerPosition[PointerPosition + Offset, ?Circular := Circular]

        AddFirst<public>(Element:t)<computes>:Container(t) = Container(t){
            PointerPosition := PointerPosition + 1
            Elements := array{Element} + Elements
        }
        AddLast<public>(Element:t)<computes>:Container(t) = Container(t){
            PointerPosition := PointerPosition
            Elements := Elements + array{Element}
        }

        # Get the value at an specific index
        # Fails if not able to find a value on the provided index
        # If Strict is set to false (Default), out of bound indexes will be normalized to array bounds
        # If Strict is set to true, out of bound indexes will not be normalized and will cause a fail
        GetAtIndex<public>(Index:int, ?Strict:logic=false)<computes><decides>:tuple(Container(t), t) = {
            TargetIndex := if (Strict?). Index else. NormalizeIndex[Elements.Length, Index]

            (Container(t){
                PointerPosition := if (PointerPosition < TargetIndex). PointerPosition else. Max(0, Min(PointerPosition - 1, Elements.Length - 2))
                Elements := Elements.RemoveElement[TargetIndex]
            }, Elements[TargetIndex])
        }

        GetAtPointer<public>()<computes><decides>:t = Elements[PointerPosition]

        GetFirst<public>()<computes><decides>:tuple(Container(t), t) = (
            Container(t){
                PointerPosition := Max(PointerPosition - 1, 0)
                Elements := Elements.RemoveElement[0]
            },
            Elements[0]
        )
        
        GetLast<public>()<computes><decides>:tuple(Container(t), t) = (
            Container(t){
                PointerPosition := Max(0, Min(PointerPosition, Elements.Length - 1))
                Elements := Elements.RemoveElement[Elements.Length - 1]
            },
            Elements[Elements.Length - 1]
        )

        ### WIP Future Planned Features:
        # AddAt
        # Find
        # FindAll
        # Reduce ?
        # Splice

        # `(_Self:Container(t))` is a fix for (local:) not being allowed in class scope
        (_Self:Container(t)).SortBy<public>(Less:type {_(:t, :t)<computes><decides>:void})<computes>:Container(t) = Container(t){
            PointerPosition := PointerPosition
            Elements := SortBy(Elements, Less)
        }

        # Aliases
        PreviousPointer<public>(?Circular:logic = false)<computes><decides>:Container(t) = SetPointerOffset[-1, ?Circular := Circular]
        NextPointer<public>(?Circular:logic = false)<computes><decides>:Container(t) = SetPointerOffset[1, ?Circular := Circular]

        Push<public>(Element:t)<computes>:Container(t) = AddLast(Element)
        Pop<public>()<computes><decides>:tuple(Container(t), t) = GetLast[]

        Unshift<public>(Element:t)<computes>:Container(t) = AddFirst(Element)
        Shift<public>()<computes><decides>:tuple(Container(t), t) = GetFirst[]
    }
}