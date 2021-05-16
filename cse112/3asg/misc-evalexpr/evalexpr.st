#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/smalltalk/bin/gst -f
"$Id: evalexpr.st,v 1.1 2021-02-17 20:04:39-08 - - $"

Object subclass: Expr [
   nan := 0.0 / 0.0.
   printOn1: file [^ file << '(' << self class << ' ']
   prefix [^ '(', self class printString, ' ']
   value [^ nan]
].

Expr subclass: NumExpr [
   |value|
   NumExpr class >> new: val [^ super new init: val]
   init: val [value := val + 0.0]
   printOn: file [^ file << self prefix << value << ')']
   value [^ value]
].

Expr subclass: VarExpr [
   varDict := Dictionary from: {
      #pi   -> -1.0 arcCos.
      #e    -> 1.0 exp.
      #i    -> -1.0 sqrt.
      #nan  -> (0.0 / 0.0).
      #one  -> 1.0.
      #zero -> 0.0.
   }.
   |variable|
   VarExpr class >> new: var [^ super new init: var]
   init: var [variable := var]
   printOn: file [^ file << self prefix << variable << ')']
   value [^ varDict at: variable ifAbsent: [0.0]]
].

Expr subclass: UnopExpr [
   |oper opnd|
   UnopExpr class >> new: op with: arg [^ super new init: op with: arg]
   init: op with: arg [oper := op. opnd := arg]
   printOn: file [^ file << self prefix << oper << ' ' << opnd << ')']
   value [^ opnd value perform: oper]
].

Expr subclass: BinopExpr [
   |oper opnd1 opnd2|
   BinopExpr class >> new: op with: arg1 with: arg2 [
      ^ super new init: op with: arg1 with: arg2
   ]
   init: op with: arg1 with: arg2 [
      oper := op. opnd1 := arg1. opnd2 := arg2.
   ]
   printOn: file [
      ^ file << self prefix << oper << ' ' << opnd1
             << ' ' << opnd2 << ')'
   ]
   value [^ opnd1 value perform: oper with: opnd2 value]
].

Expr extend [
   Expr class >> parse: expr [
      expr isNumber ifTrue: [^ NumExpr new: expr].
      expr isSymbol ifTrue: [^ VarExpr new: expr].
      expr isArray ifTrue: [
         expr size = 2 ifTrue: [
            ^ UnopExpr new: (expr at: 1)
                       with: (Expr parse: (expr at: 2)).
         ].
         expr size = 3 ifTrue: [
            ^ BinopExpr new: (expr at: 1)
                        with: (Expr parse: (expr at: 2))
                        with: (Expr parse: (expr at: 3)).
         ].
      ].
      stdout << 'Unimplemented: ' << expr << Character nl.
      ObjectMemory quit: 1.
   ]
]

Object subclass: Tester [
   nl := Character nl.
   Tester class >> test: expr [
      |object|
      stdout << nl.
      stdout << 'expr: ' << expr << nl.
      object := Expr parse: expr.
      stdout << 'object: ' << object << nl.
      stdout << 'value: ' << object value << nl.
   ]
].

Tester test: 3.
Tester test: #(#/ 1 2).
Tester test: #zero.
Tester test: #pi.
Tester test: #(#+ #(#* 3 4) #(#* 5 6)).
Tester test: #(#+ #(#raisedTo: #e #(#* #i #pi)) 1).
Tester test: #(#/ #zero #zero).
Tester test: #(#/ #one #zero).
Tester test: #(#sqrt -1).
Tester test: #(#arcCos -1).
Tester test: #(#exp 1).

