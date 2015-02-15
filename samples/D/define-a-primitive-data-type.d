import std.exception, std.string, std.traits;

/++
A bounded integral type template.
Template params:
- min: the minimal value
- max: the maximal value
- I: the type used to store the value internally
+/
struct BoundedInt(alias min, alias max, I = int)
{
    // Static checks
    static assert(isIntegral!(typeof(min)));
    static assert(isIntegral!(typeof(max)));
    static assert(isIntegral!I);
    static assert(min < max);
    static assert(cast(I) max == max, "Type " ~ I.stringof
        ~ " cannot hold values up to " ~ max.stringof);

    /// The actual value stored in this struct
    private I _value;

    /// Getter for the internal value
    @property I internalValue()
    {
        return _value;
    }

    /// 'alias this' to make this struct look like a built-in integer
    alias internalValue this;

    /// Constructor
    this(T)(T value)
    {
        opAssign(value);
    }

    /// Assignment operator
    void opAssign(T)(T value)
        if (isIntegral!T)
    {
        _value = checked(value);
    }

    /// Unary operators
    auto opUnary(string op)() const
    {
        return checked(mixin(op ~ "_value"));
    }

    /// Binary operators
    auto opBinary(string op, T)(T other) const
    {
        return checked(mixin("_value" ~ op ~ "other"));
    }

    /// ditto
    auto opBinaryRight(string op, T)(T other) const
        if (isIntegral!T)
    {
        return checked(mixin("_value" ~ op ~ "other"));
    }

    // Bounds enforcement
    private I checked(T)(T value) const
    {
        enforce(value >= min && value <= max,
            format("Value %s is out of bounds (%s to %s).", value, min, max));
        return cast(I) value;
    }
}

unittest
{
    alias MyInt = BoundedInt!(1, 10);
    // alias BoundInt!(1, 10) MyInt; // D < 2.061

    MyInt i = 4;
    MyInt j = i + i;
    assert(j / 2 == 4);
    assert(2 + j == 10);
    assert(i < 5);
    assert(j > i);
}
