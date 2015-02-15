numbers = [1, 3, 5, 7]

square1 = [square(n) for n in numbers]     # list comprehension

squares2a = map(square, numbers)            # functional form

squares2b = map(x -> x*x, numbers)     # functional form with `lambda`

#There is also extended block form for the map function
squares2c = map(numbers) do x
	sum = 0
	for i = 1:x
		sum += x^2				#trivial, but you get the point
	end
	return sum
end

squares3 = [n * n for n in numbers]         # no need for a function,

squares4 = numbers .* numbers				# element-wise operation

squares4a = numbers .^ 2	#  most arithmetic operations can be done element-wise
