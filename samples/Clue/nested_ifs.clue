if condition {
	if condition2 {
		print("condition2")
	} else {
		print("condition")
	}
} else {
	print("no condition")
}

//can also be written as
match condition {
	true if condition2 => {
		print("condition2")
	}
	true => {
		print("condition")
	}
	false => {
		print("no condition")
	}
}