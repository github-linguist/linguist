zeroDivide := [:aBlock |
	[aBlock value. false] on: ZeroDivide do: [true].
	].

"Testing"
zeroDivide value: [2/1] "------> false"
zeroDivide value: [2/0] "------> true"
