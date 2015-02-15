function gcd(a,b)
	if b ~= 0 then
		return gcd(b, a % b)
	else
		return math.abs(a)
	end
end

function demo(a,b)
	print("GCD of " .. a .. " and " .. b .. " is " .. gcd(a, b))
end

demo(100, 5)
demo(5, 100)
demo(7, 23)
