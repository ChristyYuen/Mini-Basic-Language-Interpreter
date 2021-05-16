#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/smalltalk/bin/gst -f
"$Id: readnumbers.st,v 1.3 2020-11-09 22:09:39-08 - - $"

Object subclass: NumberStream [
   |atEndFlag stream buffer|
   NumberStream class >> new [
      ^ super new init: stdin.
   ]
   NumberStream class >> new: aStream [
      ^ super new init: aStream.
   ]
   init: aStream [
      atEndFlag := false.
      stream := aStream.
      buffer := OrderedCollection new.
   ]
   atEnd [ ^ atEndFlag ]
   nextNumber [
      [atEndFlag] whileFalse: [
         buffer size > 0 ifTrue: [
            ^ buffer removeFirst asNumber asFloatD.
         ].
         stdout flush.
         stderr flush.
         stream atEnd "This condition fills the input buffer."
            ifTrue: [atEndFlag := true]
            ifFalse: [buffer := stream nextLine substrings: ' ']
      ].
      ^ 0.0 / 0.0 "NaN".
   ]
]

input := NumberStream new.
nl := Character nl.
[input atEnd] whileFalse: [
   |num|
   num := input nextNumber.
   stdout << num class << ': ' << num << nl.
]

