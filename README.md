# Flax Functional
A small functional language compiler.

## How to build
With Dune: 
```shell
dune build
```

With Opam:
```shell
opam update
opam install . --deps-only
```

#### Run test suite
```shell
dune build
dune runtest
```


## Design
The overall design of the compiler includes the following steps
```
Lexer -> Parser -> Desugarer -> CPS Transformer -> Alpha Conversion -> FreeVar Transformer -> Lambda Lifter -> CodeGen
```
- Lexing and parsing are done using `OCamllex` and `Menhir`. This then returns an AST (Abstract Syntax Tree) containing the `Source Grammar`.
- The `Source Grammar` is then passed to the `Desugarer` and is desugared into the `Core Grammar`. 
- The `Core Grammar` is the final AST representation if the program. This is then passed though transformation steps to further optimize the program. This includes the `CPS` transformation.
- `CPS Transformer` converts the program to continuation passing style in order to make every function in tail call position
- `Alpha Conversion` Computes unique variable names for all variables in the program
- The `FreeVar Transformer` computes all the free variables in the program. This is need for Lambda Lifting
- The `Lambda Lifter` restructures the program so that functions are defined independently of each other in a global scope
- The final step is to convert the program into machine code, which is done in `CodeGen`.


## More Info
- [Source and Core Grammar](./doc/grammar.md)
- [Desugaring](./doc/desugar.md)