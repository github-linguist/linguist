PackageLoader fileInPackage: 'TCP'!

Object subclass: #HelloSocket
  instanceVariableNames: 'ss'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'SimpleEcho'!

!HelloSocket class methodsFor: 'instance creation'!

port: anInteger
  | ses |
  ses := super new.
  ses init: anInteger.
  ^ses
!!

!HelloSocket methodsFor: 'instance initialization'!

init: anInteger
  ss := (TCP.ServerSocket port: anInteger).
  ^self
!!

!HelloSocket methodsFor: 'running'!

run
  | s |
  [
    ss waitForConnection.
    s := (ss accept).
    [self handleSocket: s] fork
  ] repeat
!!

!HelloSocket methodsFor: 'handling'!

handleSocket: s
    | msg |
    msg := 'hello socket world'.
    msg displayOn: s.
    (String with: (Character value: 10)) displayOn: s.
    s flush
!!

Smalltalk at: #helloServer put: (HelloSocket port: 2560).

helloServer run.
