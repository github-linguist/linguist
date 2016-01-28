local struct Type {
        field: int
}

local terra foo(obj: Type) end
foo:setinlined(false)

local terra bar()
        var obj: Type
        defer foo(obj)
        return
end

bar()