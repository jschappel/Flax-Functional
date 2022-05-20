

## Source Grammer
```shell
Program ::= <Def>+

Def       ::= "(" "define" VAR <Exp> ")"
            | "(" "define" "(" VAR VAR* ")" <Exp> ")"
            | <Var> <Var>*  #TODO: Finish this

Exp       ::= <NumExp>
            | <BoolExp>
            | <SymExp>
            | <StrExp>
            | <VarExp>
            | <IfExp>
            | <CondExp>
            | <LambdaExp>
            | <LetExp>
            | <LetrecExp>
            | <AndExp>
            | <OrExp>
            | <NotExp>
            | <AppExp>
            | <VectorExp>
            | <ListExp>
            | <SetExp>
            | <BeginExp>

NumExp    ::= NUMBER

BoolExp   ::= "true" | "false"

SymExp    ::= "'" VAR

StrExp    ::= "\"" (VAR | NUMBER)* "\""

VarExp    ::= VAR

IfExp     ::= "(" <Exp> <Exp> <Exp> ")"

CondExp   ::= "(" cond ("(" <Exp> <Exp> ")")+ ")"

LambdaExp ::= "(" "lambda" "(" <Var>* ")" <Exp> ")"

LetExp    ::= "(" "let" "(" ("(" <Var> <Exp> ")")* ")" <Exp> ")"

LetrecExp ::= <Def>* <Exp>

LetrecExp ::= "(" "letrec" "(" ("(" <Var> <Exp> ")")* ")" <Exp> ")"

AndExp    ::= "(" "and" <Exp>* ")"

OrExp     ::= "(" "or" <Exp>* ")"

NotExp    ::= "(" "not" <Exp> ")"

AppExp    ::= "(" <Exp> <Exp>* ")"

vectorexp ::= "(" "vector" <Exp>* ")"

ListExp   ::= "(" "list" <Exp>* ")"

SetExp    ::= "(" "set" <Var> <Exp> ")"

BeginExp  ::= "(" "begin" <Exp>* ")"  
```

where `CondExp` and `LetExp` are desugared.


## Core Grammar
```shell
CoreProgram ::= <CoreDef>+

CoreDef         ::=  CoreVar <CoreExp> 

CoreExp         ::= <CoreNumExp>
                  | <CoreBoolExp>
                  | <CoreSymExp>
                  | <CoreStrExp>
                  | <CoreVarExp>
                  | <CoreIfExp>
                  | <CoreCondExp>
                  | <CoreLambdaExp>
                  | <CoreLetExp>
                  | <CoreLetrecExp>
                  | <CoreAndExp>
                  | <CoreOrExp>
                  | <CoreNotExp>
                  | <CoreAppExp>
                  | <CoreVectorExp>
                  | <CoreListExp>
                  | <CoreSetExp>
                  | <CoreBeginExp>

CoreNumExp      ::= NUMBER

CoreBoolExp     ::= BOOL

CoreSymExp      ::= CoreVar

CoreStrExp      ::= STRING

CoreVarExp      ::= CoreVar

CoreFreeVarExp  ::= CoreVar Number

CoreIfExp       ::= <CoreExp> <CoreExp> <CoreExp> 

CoreLambdaExp   ::=  <CoreVar>*  <CoreExp> 

CoreLetExp      ::=  (<CoreLetDeclExp>)*  <CoreExp>

CoreLetDeclExp  ::= CoreVar CoreExp

CoreAndExp      ::= <CoreExp>* 

CoreOrExp       ::= <CoreExp>* 

CoreNotExp      ::= <CoreExp>

CoreAppExp      ::= <CoreExp> <CoreExp>*

CoreVectorexp   ::= <CoreExp>*

CoreListExp     ::= <Exp>*

CoreSetExp      ::= <Var> <Exp>

CoreBeginExp    ::= <Exp>* 
```