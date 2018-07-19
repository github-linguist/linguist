// works for Rust 0.9
fn entropy(s: &str) -> f32 {
	let mut entropy: f32 = 0.0;
	let mut histogram = [0, ..256];
	let len = s.len();

	for i in range(0, len) { histogram[s[i]] += 1; }
	for i in range(0, 256) {
		if histogram[i] > 0 {
			let ratio = (histogram[i] as f32 / len as f32) as f32;
			entropy -= (ratio * log2(ratio)) as f32;
		}
	}

	entropy
}
