"======================================================================
|
|   Smalltalk dining philosophers
|
|
 ======================================================================"


"======================================================================
|
| Copyright 1999, 2000 Free Software Foundation, Inc.
| Written by Paolo Bonzini.
|
| This file is part of GNU Smalltalk.
|
| GNU Smalltalk is free software; you can redistribute it and/or modify it
| under the terms of the GNU General Public License as published by the Free
| Software Foundation; either version 2, or (at your option) any later version.
| 
| GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
| ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
| FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
| details.
| 
| You should have received a copy of the GNU General Public License along with
| GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
| Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
|
 ======================================================================"


Object subclass: #Philosophers
    instanceVariableNames: 'forks philosophers randy eating'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Examples-Processes'!

!Philosophers class methodsFor: 'dining'!

new
    self shouldNotImplement
!

new: quantity
    ^super new initialize: quantity
! !

!Philosophers methodsFor: 'dining'!

dine
    self dine: 15
!

dine: seconds
    (Delay forSeconds: seconds) wait.
    philosophers do: [ :each | each terminate ].
    self initialize: self size
!

leftFork: n
    ^forks at: n
!

rightFork: n
    ^n = self size
	ifTrue: [ forks at: 1 ]
	ifFalse: [ forks at: n + 1 ]
!

initialize: n
    eating := Semaphore new.
    n - 1 timesRepeat: [ eating signal ].

    randy := Random new.
    forks := (1 to: n) collect: [ :each | Semaphore forMutualExclusion ].
    philosophers := (1 to: n) collect: [ :each | self philosopher: each ].
!

philosopher: n
    | philosopherCode leftFork rightFork status |
    leftFork  := self leftFork: n.
    rightFork := self rightFork: n.
    status    := 'Philosopher #', n printString, ' '.
    philosopherCode := [[ true ] whileTrue: [
	Transcript nextPutAll: status, 'thinks'; nl.
	(Delay forMilliseconds: randy next * 2000) wait.
	Transcript nextPutAll: status, 'wants to eat'; nl.
	eating critical: [				    "Avoid deadlock"
	    Transcript nextPutAll: status, 'waits for left fork'; nl.
	    leftFork wait.
	    Transcript nextPutAll: status, 'waits for right fork'; nl.
	    rightFork wait.
	    Transcript nextPutAll: status, 'eats'; nl.
	    (Delay forMilliseconds: randy next * 2000) wait.
	    leftFork signal.
	    rightFork signal.
	].
    ]].

    ^(philosopherCode newProcess)
	priority: Processor userBackgroundPriority;
	name: status;
	resume;
	yourself
!

size
    ^forks size
! !

(Philosophers new: 5) dine!
