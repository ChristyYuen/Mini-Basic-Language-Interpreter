.so Tmac.mm-etc
.if t .Newcentury-fonts
.INITR* \n[.F]
.SIZE 12 14
.TITLE CSE-112 Spring\~2021 Program\~3 "MB Interpreter in Smalltalk"
.RCS "$Id: asg3-smalltalk-mbst.mm,v 1.8 2021-04-28 13:43:44-07 - - $"
.PWD
.URL
.de V=BLI
.   LI "\f[CB]\\$[1]\f[P]\\$[2]\f[CB]\\$[3]\f[P]"
.   br
..
.ds CSE-112-root /afs/cats.ucsc.edu/courses/cse112-wm
.ds PROFILE \[Do]HOME/.bash_profile
.H 1 "Overview"
Smalltalk is a pure object-oriented language,
where everything is an object,
and all actions are accomplished by sending messages,
even for what in other languages would be control structures.
In this project we will be using Gnu Smalltalk,
the Smalltalk for those who can type.
References\(::
.VTCODE* 1 https://ftp.gnu.org/gnu/smalltalk/
.VTCODE* 1 https://www.gnu.org/software/smalltalk/
.VTCODE* 1 https://www.gnu.org/software/smalltalk/manual/
.VTCODE* 1 https://www.gnu.org/software/smalltalk/manual-base/
.VTCODE* 1 https://learnxinyminutes.com/docs/smalltalk/
.VTCODE* 1 http://www.angelfire.com/tx4/cus/notes/smalltalk.html
.H 1 "Running Gnu Smalltalk interactively"
Smalltalk is an interpretibve language and can be run from
the command line or by a scriptl
To run
.V= gst ,
add the following to your
.V= \*[PROFILE] \(::
.VTCODE* 1 "export PATH=$PATH:\*[CSE-112-root]/usr/smalltalk/bin"
When running
.V= gst
interactively, use the command
.V= rlwrap
to gain access to the readline arrow keys so that you
can recover earlier typed lines.
Notice the unexpected(?) operator  precedence.
Example\(::
.TVCODE* 1 "-bash-$ " "rlwrap gst"
.TVCODE* 1 "GNU Smalltalk ready"
.TVCODE* 1 "st> " "2 + 3 * 6."
.TVCODE* 1 "30"
.TVCODE* 1 "st> " "2 raisedTo: 128."
.TVCODE* 1 "340282366920938463463374607431768211456"
.TVCODE* 1 "st> " "(-1 arcCos * -1 sqrt) exp + 1."
.TVCODE* 1 "(0.0+0.00000000000000012246467991473532i)"
.TVCODE* 1 "st> " "5 sqrt + 1 / 2."
.TVCODE* 1 "1.618033988749895"
.TVCODE* 1 "st> " "\[ha]D"
.P
Instead of explicitly typing
.V= rlwrap 
every time you start 
.V= gst
interactively, create the command
.V= wgst
by putting the following in in your
.V= \*[PROFILE] \(::
.VTCODE 1 "alias wgst='rlwrap gst'"
.H 1 "The program \f[CB]mbint.st\f[P]"
Complete the implementation of
.V= mbint.st ,
a Smalltalk implementation of the Basic interpreter from 
previous projects.
The following classes are of interest\(::
.BVL \n[Pi]
.V=BLI "Object subclass: Debug"
A singleton class with a debug flag which can be turned on
from the command line.
Multiple
.V= -d
options increase the level of debugging for more information.
.V=BLI "Object subclass: MiniBasic"
The root class to hold methods that are usefully inherited
by other classes.
.V= unimplemented:
is used to print error messages for classes not fully implemented.
.V= prefix 
is used by the multiple
.V= printOn:
methods to label the output when
.V= self
is printed.
.V=BLI "MiniBasic subclass: Expr"
.V= Expr
is the abstract base class from which all other expression
classes inherit.
.V=BLI "Expr subclass: NumExpr"
Holds numbers of class
.V= FloaatD
or
.V= Complex .
Complex numbers are not specifically handled,
but may be produced by expressions such as
.V= (-1\~sqrt) .
.V=BLI "Expr subclass: VarExpr"
Each instance represents a simple variable.
The class variable
.V= varDict
holds the symbol table.
.V=BLI "Expr subclass: UnopExpr"
Implements unary operators (functions).
The
.V= value
method is to be implemented.
.V=BLI "Expr subclass: BinopExpr"
Implements binary operators.
The
.V= value
method is to be implemented.
.V=BLI "Expr subclass: ArrayExpr"
Holds the array table as a class varible and array names
as instances.
.V=BLI "Expr extend"
Is an extension of 
.V= Expr
placed later in the file so that it can refer to
its subclasses.
.V= Expr>>parse:
accepts the array prefix notation of the intermediate format
and produces the proper class instances.
.V=BLI "MiniBasic subclass: Stmt"
Is the root of the statment interpreters.
The class variable
.V= stmtNr
contains the number of the next statement to be implemented.
.V= labelDict
is to contain the labels from the original program as keys
and the corresponding statement numbers as values.
.V=BLI "Stmt subclass: DimStmt"
Interprets the
.V= dim
statement.
.V=BLI "Stmt subclass: LetStmt"
Evaluates an expression and assigns it to the appriate variable.
.V=BLI "Stmt subclass: GotoStmt"
The
.V= interp
method assign a new value to
.V= statNr.
.V=BLI "Stmt subclass: IfStmt"
Evaluates the expression.
If true, behaves like the
.V= goto
statement.
.V=BLI "Stmt subclass: InputStmt"
Reads input for each variable mentioned on the input line
and stores its value in the appropriate symbol table.
If the token read
.V= isNumber 
it is returned.
otherwise
.V= NaN
.=V ( 0.0/0.0 )
is returned.
.V=BLI "Stmt subclass: PrintStmt"
Prints the list of arguments in readable format.
.V=BLI "Stmt subclass: NullStmt"
A dummy slot used when the intermediate language has
an empty statement.
.V= interp
does nothing.
.V=BLI "Stmt extend"
Is an extension of
.V= Stmt
that can refer to the subclasses.
.V= Stmt>>parse
converts the intermediate langage into an object structure.
.V=BLI "MiniBasic subclass: Interpreter"
Initializes the progra structure and controls the
.V= Stmt
interpreters.
.V= Interpreter>>print
is for debugging only.
.V=BLI "Object subclass: Main"
Takes care of scanning the command line option and operand,
produces suitable error messages if appropriate,
and calls the interpreter.
.LE
.H 1 "What to submit"
Submit
.V= mbint.st
and
.V= README .
Also
.V= PARTNER 
if doing pair programming.
