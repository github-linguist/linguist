Roman := Object clone do (
    nums := list(1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1)
    rum := list("M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I")

    numeral := method(number,
        result := ""
        for(i, 0, nums size,
            if(number == 0, break)
            while(number >= nums at(i),
                number = number - nums at(i)
                result = result .. rum at(i)
            )
        )
        return result
    )
)

Roman numeral(1666) println
