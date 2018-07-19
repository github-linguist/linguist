ODESolver>>eulerOf: f init: y0 from: a to: b step: h
	| t y |
	t := a.
	y := y0.
	[ t < b ]
		whileTrue: [
			Transcript
				show: t asString, ' ' , (y printShowingDecimalPlaces: 3);
				cr.
			t := t + h.
			y := y + (h * (f value: t value: y)) ]

ODESolver new eulerOf: [:time :temp| -0.07 * (temp - 20)]  init: 100 from: 0 to: 100 step: 10
