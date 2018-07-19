function solve24(nums)
    length(nums) != 4 && error("Input must be a 4-element Array")
    syms = [+,-,*,/]
    for x in syms, y in syms, z in syms
        for i = 1:24
            a,b,c,d = nthperm(nums,i)
            if round(x(y(a,b),z(c,d)),5) == 24
                return "($a$y$b)$x($c$z$d)"
            elseif round(x(a,y(b,z(c,d))),5) == 24
                return "$a$x($b$y($c$z$d))"
            elseif round(x(y(z(c,d),b),a),5) == 24
                return "(($c$z$d)$y$b)$x$a"
            elseif round(x(y(b,z(c,d)),a),5) == 24
                return "($b$y($c$z$d))$x$a"
            end
        end
    end
    return "0"
end
