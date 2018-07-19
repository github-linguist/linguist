DLD addLibrary: 'fakeimglib'.

Object subclass: ExtLib [
  ExtLib class >> openimage: aString [
    (CFunctionDescriptor isFunction: 'openimage')
    ifTrue: [
       (CFunctionDescriptor for: 'openimage'
                            returning: #int
                            withArgs: #( #string ) ) callInto: (ValueHolder null).
    ] ifFalse: [ ('internal open image %1' % { aString }) displayNl ]
  ]
].

ExtLib openimage: 'test.png'.
