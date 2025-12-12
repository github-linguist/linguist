alias OurTrue = OurBool(__mlir_attr.`true`)
alias OurFalse: OurBool = __mlir_attr.`false`


@register_passable("trivial")
struct OurBool:
    var value: __mlir_type.i1

    fn __init__() -> Self:
        return OurFalse

    fn __init__(value: __mlir_type.i1) -> Self:
        return Self {value: value}

    fn __bool__(self) -> Bool:
        return Bool(self.value)

    fn __mlir_i1__(self) -> __mlir_type.i1:
        return self.value

    fn __eq__(self, rhs: OurBool) -> Self:
        let lhsIndex = __mlir_op.`index.casts`[_type : __mlir_type.index](self.value)
        let rhsIndex = __mlir_op.`index.casts`[_type : __mlir_type.index](rhs.value)
        return Self(
            __mlir_op.`index.cmp`[pred : __mlir_attr.`#index<cmp_predicate eq>`](
                lhsIndex, rhsIndex
            )
        )

    fn __invert__(self) -> Self:
        return OurFalse if self == OurTrue else OurTrue
