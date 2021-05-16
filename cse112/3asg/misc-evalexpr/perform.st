#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/smalltalk/bin/gst -f
"$Id: perform.st,v 1.1 2021-02-17 20:04:39-08 - - $"

"Example of perform: methods."

nl := Character nl.

binops := #(#+ #- #* #/ #raisedTo:).

data := #( #(2 3) #(4 5)).

data do: [:opnds|
   |left right|
   left := (opnds at: 1) asFloatD.
   right := (opnds at: 2) asFloatD.
   binops do: [:oper|
      |result|
      stdout << left << ' ' << oper << ' ' << right << ' = '.
      result := left perform: oper with: right.
      stdout << result << nl.
   ].
].

