.so Tmac.mm-etc
.if t .Newcentury-fonts
.INITR* \n[.F]
.SIZE 12 14
.TITLE CSE-112 Spring\~2021 Program\~4 "Perl and Rapid Development"
.RCS "$Id: asg4-perl-pmake.mm,v 1.54 2021-05-22 10:48:37-07 - - $"
.PWD
.URL
.H 1 "Overview"
Scripting is a style of programming whereby small programs 
are developed rapidly.
This is also sometimes called rapid prototyping.
Perl is a language which supports this particular programming
paradigm very well because it is a very powerful and interpreted
language.
There is no need to do the usual compile-run cycle, since
the program is compiled every time it is run.
.P
The
.V= make (1)
utility determines automatically which pieces of a
large program need to be recompiled, and issue the commands  to
recompile them.
This project will also enhance your knowledge of
.V= make
and
.V= Makefile s,
as presented in prerequisite courses.
Every programmer should have a detailed knowledge of
.V= make .
.H 1 "An implementation of a subset of make"
In this assignment, you will use Perl to write a replacement for
a subset of
.V= make .
.SH=BVL
.B=LI "NAME"
pmake \[em] perl implementation of a subset of make
.B=LI "SYNOPSIS"
.V= pmake
.=V \|[ -d ]
.RI \|[ target ]
.B=LI "DESCRIPTION"
The
.V= pmake
utility executes a list of shell commands associated with each
.IR target ,
typically to create or update files of the same name.
The
.V= Makefile
contains entries that describe how to bring a target up to date with
respect to those on which it depends,
which are called prerequisites.
.B=LI "OPTIONS"
.br
The following options are supported.
All options must precede all operands,
and all options are scanned by 
.V= Getopt::Std::getopts
(perldoc).
.VL \n(Pi
.LI "\f[CB]-d\f[R]"
Displays debugging information.
Output is readable only to the implementor.
Other debugging flags may also be added,
but none are production flags.
.LE
.B=LI "OPERANDS"
The following operand is recognized.
.VL \n(Pi
.LI "\f[I]target\f[R]
.br
An attempt is made to build each target in sequence in the
order they are given on the command line.
If no target is specified, the first target in the
.V= Makefile
is built.
This is usually, but not necessarily, the target
.V= all .
.LE
.B=LI "FILES"
Reads the file called
.V= Makefile
in the current directory.
If the file does not exist,
.V= pmake
exits with an error message.
.br
.ne 4
.B=LI "EXIT STATUS"
.VL \n[Pi]
.LI 0
No errors were detected.
.LI 1
An error in the
.V= Makefile
was detected.
Or if any subprocess returned a non-zero exit status
or failed on a signal,
and the command was not preceded by the minus sign
.=V ( - )
marker.
.LE
.br
.ne 6
.B=LI "MAKEFILE SYNTAX"
.br
Generally, whitespace delimits words,
but in addition, punctuation is recognized as well.
Each line of input is a comment, an empty line, a dependency,
or a command.
.VL \n[Pi]
.LI \f[CB]#\fR
Any line with begins with a hash, possibly preceded by whitespace
(spaces and tabs) is ignored.
Empty lines consisting only of whitespace are also ignored.
.LI
\f[I]macro\f[R]\|\f[CB]=\f[R]\|\f[I]value\f[R]
.br
Macro definitions are kept in a symbol (hash) table, 
to be substituted later.
.LI
\fItarget\fR\|.\|.\|. \f[CB]:\fR \fIprerequisite\fR\|.\|.\|.
.br
Each target's time stamp is checked against the time stamps of
each of the prerequisites.
If the target or prerequisite contains a percent sign
.=V ( % ),
it is substituted consistently.
If any target is obsolete, the following commands are executed.
A target is obsolete if it is a file that is older than the 
prerequisites or does not exist.
A prerequisite is either a file or another target.
If a file, its time stamp is checked.
If not, the target to which is refers is made recursively.
No target is made more than once.
.LI \fIcommand\fR
.br
A command is any line for which the first character is a tab .
The line is echoed to 
.V= STDOUT
before executing the command.
The line is then passed to the
.V= system
function call for execution by the shell.
The resulting exit status and signal is then tested.
If either is non-zero, 
.V= pmake
exits at that point.
.LI
\f[CB]@\fP \fIcommand\fR
.br
Behaves like
.IR command ,
except that the command is not echoed to
.V= STDOUT
before being executed.
.LI
\f[CB]-\fP \fIcommand\fR
.br
Behaves like
.IR command ,
except that a non-zero exit status or signal does not cause
.V= pmake
to exit at that point.
.LE
.B=LI MACROS
Whenever a dollar sign appears in the input file,
it represents a macro substitution.
Macros are substituted from innermost to outermost braces.
If a dollar sign is followed by any character except a left brace
that one character is the macro name.
Otherwise, the characters between the braces constitute the
name of the macro.
.nr Pi2 \n[Pi]*2
.VL \n[Pi2]
.LI \f[CB]\[Do]\[Do]\fR
Represents the dollar sign itself.
.LI \f[CB]\[Do]<\fR
Represents the first file specified as a prerequisite.
.LI \f[CB]\[Do]{\fR\|.\|.\|.\|\f[CB]}\fR
The contents of the braces are substituted with the value of the
macro name, which may be multiple characters,
not including a closing brace.
.LE
.LE
.H 1 Commentary
Here are some hints that will be useful in familiarizing yourself with
Perl and how to perform certain kinds of coding.
.ALX a ()
.LI
The directory
.V= /afs/cats.ucsc.edu/courses/cse112-wm/bin
contains examples of Perl scripts.
And the subdirectory
.V= code/
of this directory contains relevant code.
.LI
The function
.V= system
will pass a command string to the shell and set the variable
.V= \[Do]?
to the
.V= wait (2)
return value.
If the termination signal is 0 (bits 6...0),
then the program exited normally and bits 15...8 contain the
.V= exit (2)
status returned by the program.
Otherwise, bits 6...0 contain 
the signal that caused the program to terminate,
and bit 7 indicates whether or not core was dumped.
The following code can be used to extract this information\(::
.DS
.VCODE* 1 "my \[Do]term_signal = \[Do]? & 0x7F;"
.VCODE* 1 "my \[Do]core_dumped = \[Do]? & 0x80;"
.VCODE* 1 "my \[Do]exit_status = (\[Do]? >> 8) & 0xFF;"
.DE
.LI
A C++ program
.V= code/sigtoperl.cpp
prints out a description of all of the signals.
The output is in 
.V= code/perlsignals.out
This output may be inserted into your Perl program.
.LI
Use the function
.V= system
to run the command.
.V= \[Do]?
is the
.V= wait (2)
exit status.
The notation
.V= wait (2)
refers to the manual
page in section 2 of the manual.
The command may be read with the command
.VTCODE* 1 "man -s 2 wait"
.LI
Keep all macros in a hash table.
.br
.ne 6
.LI
To extract the innermost macro substitution,
the following pattern will avoid nested macros\(::
.V= \[rs]\[Do]{[\(ha}]+} .
Alternately, you may wish to parse macro lines into an AST
matching braces.
Remember that regular expressions don't handle matched structures
but context free grammars do.
.br
.ne 5
.LI
Keep each target in a hash with the prerequisites and commands as a
reference to a list.
Hashes are used in Perl to represent structs.
Thus, the following will point
.V= \[Do]p
at a struct with two fields\(::
.DS
.VCODE* 1 "\[Do]p = {FOO=> 3, BAR=> [1, 2, 3]}"
.DE
.LI
The
.V= stat
function returns a list of file attributes.
The modification time is the value of interest when comparing
time stamps on files.
See
.V= perlfunc (1).
.DS
.VCODE* 1 "@filestat = stat $filename;"
.VCODE* 1 "my $mtime = $filestat[9];"
.DE
.LI
Look at the subdirectories
.V= .score/test*
and see what 
.V= make
does with them.
.LE
.H 1 "Pseudocode for make_goal"
For details of the exact format of error messages,
run
.V= make
on similar programs and copy the format.
.P
Make_goal is a recursive function.
.ALX a ()
.LI
If the goal has already been visited, return.
.br
Otherwise mark it as having been visited and continue.
.LI
If goal is not a target\(::
.ALX 1 () "" 0
.LI
If goal is a file, return its modtime time.
.LI
Else print a message ``No rule to make...'' and exit 1.
.LE
.LI
If it is a target (even if a file of the same name exists),
continue.
.br
For each prerequisite\(::
.ALX 1 () "" 0
.LI
Call make_goal recursively for the prerequisite.
.LI
Remember the newest prerequisite modtime of all of them.
.LI
If there are no prerequisites, the modtime for them is 0.
.LE
.LI
If either of the following is true, run the commands.
.ALX 1 () "" 0
.LI
The target file does not exist.
.LI
The newest prerequisite is newer than the target modtime.
.LE
.LI
If the commands did build the target file, return its modtime.
.br
Else return 0 as the modtime.
.LE
.H 1 "Running a command"
.ALX a ()
.LI
Perform macro substitution on the command.
.LI
If the command does not begin with a ``@'', print it.
.LI
Call run_command and check its result.
.ALX 1 () "" 0
.LI
If the result is undef, return.
.LI
If the result tests true,
and the command begins with a ``\-'',
.br
print a message with ``ignored'' as part of the message, and return.
.LI
Otherwise print the message, and exit 1.
.LE
.LE
.H 1 "What to submit"
Submit one file, specifically called
.V= pmake ,
which has been
.V= chmod ed
to executable
.=V ( +x ).
Also submit
.V= README
as specified under ``pair programming''
in the syllabus.
The first line must be a hashbang for Perl.
Be sure there are no carriage returns in the file.
Also, use
.V= strict
and
.V= warnings .
Your name must come
.E= after
the hashbang line.
Grading will be done by naming it as a shell script.
Do not run it by typing the word
.V= perl
as the first word on the command line.
The first few lines are\(::
.DS
.VCODE* 1 "#!/usr/bin/perl"
.VTCODE* 1 "# " "Your name and username@ucsc.edu"
.VCODE* 1 "use strict;"
.VCODE* 1 "use warnings;"
.DE
.P
If you are doing pair programming, 
submit
.V= PARTNER
as required by the pair programming instructions in
.V= cse112-wm/Syllabus/pair-programming .
