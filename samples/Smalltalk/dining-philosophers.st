'From Squeak3.7 of ''4 September 2004'' [latest update: #5989] on 13 October 2011 at 2:44:42 pm'!
Object subclass: #Philosopher
	instanceVariableNames: 'table random name seat forks running'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'rosettacode'!

!Philosopher methodsFor: 'private'!
createfork
	^ Semaphore forMutualExclusion! !

!Philosopher methodsFor: 'private'!
displayState: aStateName
	Transcript show: name , ' is ' , aStateName;
		 cr! !

!Philosopher methodsFor: 'private'!
pickUpForkAt: relativePosition
	| fork pos |
	pos := self tableIndex: seat + relativePosition.
	(fork := table at: pos)
		ifNotNil: [fork
				critical: [(table at: pos) notNil
						ifTrue: [table at: pos put: nil]
						ifFalse: [fork := nil]]].
	^ (forks at: relativePosition put: fork) notNil! !

!Philosopher methodsFor: 'private'!
putBackForkAt: aRelativePosition
	| fork |
	fork := forks at: aRelativePosition.
	fork
		ifNotNil: [table
				at: (self tableIndex: seat + aRelativePosition)
				put: fork.
			forks at: aRelativePosition put: nil]! !

!Philosopher methodsFor: 'private'!
tableIndex: aNum
	^ aNum - 1 \\ table size + 1! !

!Philosopher methodsFor: 'private'!
waitRandomTime
	(Delay forMilliseconds: (random next * 4000) rounded) wait! !


!Philosopher methodsFor: 'dining'!
eat
	self displayState: 'eating';
		 waitRandomTime;
		 putBackForkAt: -1;
		 putBackForkAt: 1! !

!Philosopher methodsFor: 'dining'!
pickUpForks
	self displayState: 'trying to pick up forks'.
	[(self pickUpForkAt: -1)
		ifTrue: [(self pickUpForkAt: 1)
				ifFalse: [self putBackForkAt: -1]].
	(forks at: 1) notNil]
		whileFalse: [(Delay forMilliseconds: 10) wait]! !

!Philosopher methodsFor: 'dining'!
think
	self displayState: 'thinking';
		 waitRandomTime! !


!Philosopher methodsFor: 'initialize-release'!
beginDining: aName at: aTable
	name := aName.
	table := aTable.
	forks := Dictionary new at: -1 put: nil;
				 at: 1 put: nil;
				 yourself.
	random := Random new seed: name hash.
	seat := table size + 1.
	table add: self;
		 add: self createfork.
	running := true.
	[(Delay forSeconds: 20) wait.
	running := false] fork.
	[[running]
		whileTrue: [self think; pickUpForks; eat].
	nil] fork! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Philosopher class
	instanceVariableNames: ''!

!Philosopher class methodsFor: 'examples'!
diningPhilosophersTest
	| diningTable |
	diningTable := OrderedCollection new.
	#('Aristotle' 'Kant' 'Buddha' 'Marx' 'Russel' )
		do: [:aName | Philosopher new beginDining: aName at: diningTable]! !
