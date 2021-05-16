#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/smalltalk/bin/gst -f
"$Id: euler.st,v 1.1 2021-04-28 13:52:03-07 - - $"

i := -1 sqrt.
pi := -1 arcCos.
euler := (i * pi) exp + 1.

nl := Character nl.
stdout << 'i = ' << i << nl.
stdout << 'pi = ' << pi << nl.
stdout << 'euler = ' << euler << nl.

