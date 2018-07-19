OrderedCollection extend [
    swap: a and: b [
	|t|
	t := self at: a.
	self at: a put: (self at: b).
	self at: b put: t
    ]
]
