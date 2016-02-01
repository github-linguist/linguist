


terra foo()

end


terra bar()
	foo()
	var what = foo
	--what()
end

bar:compile()

