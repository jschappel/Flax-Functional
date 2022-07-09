

## Source Grammar
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

LetrecExp ::= "(" "letrec" "(" ("(" <Def> ")")* ")" <Exp> ")"

AndExp    ::= "(" "and" <Exp>* ")"

OrExp     ::= "(" "or" <Exp>* ")"

NotExp    ::= "(" "not" <Exp> ")"

AppExp    ::= "(" <Exp> <Exp>* ")"

vectorexp ::= "(" "vector" <Exp>* ")"

ListExp   ::= "(" "list" <Exp>* ")"

SetExp    ::= "(" "set" <Var> <Exp> ")"

BeginExp  ::= "(" "begin" <Exp>* ")"  
```
where `CondExp`, `ListExp`, `VectorExp` and `LetExp` are desugared.


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

CoreLambdaExp   ::=  <CoreVar>*  <CoreExp> <CoreVar>*

CoreAndExp      ::= <CoreExp>* 

CoreOrExp       ::= <CoreExp>* 

CoreNotExp      ::= <CoreExp>

CoreAppExp      ::= <CoreExp> <CoreExp>*

CoreVectorExp   ::= <CoreExp>*

CoreListExp     ::= <Exp>*

CoreSetExp      ::= <Var> <Exp>

CoreBeginExp    ::= <Exp>* 
```

where `CoreFreeVarExp` is introduced during `Free Var Transformation`


Grammar Details below:
- Grammar constant exprs: `CoreNumExp` `CoreBoolExp` `CoreStrExp` `CoreSymExp`
- `CoreVarExp` includes a string that is the name of the variable
- `CoreFreeVarExp` is a contains a string representing the name of the free
  variable, as well as a unsigned int representing the position of the free
  variable in the closure.
- `CoreIfExp` includes 3 expressions: 
  - A boolean test Expression
  - A expression that is evaluated if true 
  - A expression that is evaluated if false
- `CoreLambdaExp` contains a list of strings that represents the parameters of the
  lambda, an expression for the body of the lambda, and a list of strings that
  represents the free variables of the lambda.
- `CoreAndExp` is a list of boolean expressions
- `CoreOrExp` is a list of boolean expressions
- `CoreNotExp` is an expression to be negated
- `CoreAppExp` is an expression representing the function that is going to be
  applied, as well as a list of expressions for the input values.
- `CoreVectorExp` is a list of expressions that will be placed in the vector
- `CoreListExp` is a list of expressions that will be placed in the list
- `CoreSetExp` is a variable identifier that represents a variable to be mutated,
  and an expressions that represents the new value the variable should be changed
  to.
- `CoreBeginExp` is a list of expressions representing the expressions that are
  evaluated in the begin
- `CoreFreeVarExp` is a variables that occurs free within a given context