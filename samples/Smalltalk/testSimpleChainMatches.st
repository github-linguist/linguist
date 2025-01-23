tests
testSimpleChainMatches
	|e eCtrl |
	e := self eventKey: $e.
	eCtrl := self eventKey: $e ctrl: true.
	
	self assert: (($e ctrl, $e) matches: {eCtrl}).
	self assert: ($e ctrl matches: {eCtrl. e}).
	
	self deny: (($e ctrl, $e) matches: {eCtrl. self eventKey: $a}).
	self deny: ($e ctrl matches: {e}).