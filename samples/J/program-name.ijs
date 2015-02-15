program =: monad : 0
	if. (#ARGV) > 1 do.
		> 1 { ARGV
	else.
		'Interpreted'
	end.
)

echo 'Program: ', program 0

exit ''
