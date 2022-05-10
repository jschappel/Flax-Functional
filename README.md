# Flax Functional
A small functional language compiler to work on implementing transformations and code gen.


## Design
The overall design of the compiler includes the following steps
```
lexer -> Parser -> Desugarer -> CPS -> CodeGen
```
- Lexing and parsing are done using `OCamllex` and `Menhir`. This then returns an AST (Abstract Syntax Tree) containing the `Source Grammar`.
- The `Source Grammar` is then passed to the `Desugarer` and is desugared into the `Core Grammar`. 
- The `Core Grammar` is the final AST representation if the program. This is then passed though transformation steps to further optimize the program. This includes the `CPS` transformation.
- The final step is to convert the program into machine code, which is done in `CodeGen`.
