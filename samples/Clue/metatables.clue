local vector = {
	x = 0, y = 0,
	meta unary = fn(t) {
		return {-t.x, -t.y}
	}
	meta tostring = fn(t) {
		return "vector(" .. t.x .. ", " .. t.y .. ")"
	}
	meta + = fn(a, b) {
		return {a.x + b.x, a.y + b.y}
	}
	meta - = fn(a, b) {
		return {a.x - b.x, a.y - b.y}
	}
	meta * = fn(a, b) {
		return {a.x * b.x, a.y * b.y}
	}
	meta / = fn(a, b) {
        return {a.x / b.x, a.y / b.y}
    }
	meta == = fn(a, b) {
		return a.x == b.x && a.y == b.y
	}
}