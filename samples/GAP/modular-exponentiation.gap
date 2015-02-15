# Built-in
a := 2988348162058574136915891421498819466320163312926952423791023078876139;
b := 2351399303373464486466122544523690094744975233415544072992656881240319;
PowerModInt(a, b, 10^40);
1527229998585248450016808958343740453059

# Implementation
PowerModAlt := function(a, n, m)
    local r;
    r := 1;
    while n > 0 do
        if IsOddInt(n) then
            r := RemInt(r*a, m);
        fi;
        n := QuoInt(n, 2);
        a := RemInt(a*a, m);
    od;
    return r;
end;

PowerModAlt(a, b, 10^40);
