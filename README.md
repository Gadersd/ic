# A Rust-Based Interaction Combinator Runtime

IC is a runtime inspired by HOC’s HVM2, that leverages interaction combinators as its computational model. A compiler for UnRust, an untyped language inspired by Rust, is partially implemented.

## What are Interaction Combinators?

Interaction combinators are a model of computation that applies graph rewrite rules to three primary constructs:

- **Eraser** (`*`)
- **Duplicator** (`{}`)
- **Constructor** (`()`)

These combinators operate as the fundamental components of computation, akin to how functions work in traditional models like the lambda calculus. In fact, a subset of the lambda calculus can be translated directly into interaction combinator networks. Most functions used in practice are within this subset.

The key features of interaction combinators include:
- **Beta-optimality:** This means that function applications are performed in the most optimal way possible, avoiding unnecessary computations.
- **Automatic parallelization:** Any algorithm that is inherently parallel can automatically be distributed across multiple cores when run on the interaction combinator runtime, without requiring explicit multithreading constructs.

### Graph Rewriting Rules

The runtime evaluates interaction combinator expressions using graph rewrite rules. For an overview of the syntax used in the language, refer to the following grammar description:

```plaintext
<Node> ::=
    | "*"                     -- (ERA)ser
    | "@" <alphanumeric>       -- (REF)erence
    | <Numeric>                -- (NUM)eric
    | "(" <Tree> <Tree> ")"    -- (CON)structor
    | "{" <Tree> <Tree> "}"    -- (DUP)licator
    | "${" <Tree> <Tree> "}"   -- (OPE)rator
    | "?(" <Tree> <Tree> ")"   -- (SWI)tch

<Tree> ::=
    | <alphanumeric>           -- (VAR)iable
    | <Node>

<alphanumeric> ::= [a-zA-Z0-9_./-]+
```
from the HVM2 paper, as described in "HVM2: A PARALLEL EVALUATOR FOR INTERACTION COMBINATORS" by Victor Taelin, available at https://github.com/HigherOrderCO/HVM/blob/main/paper/HVM2.pdf.

### Example Usage

To run the project, you'll need **cargo nightly**. Make sure to have Rust's nightly toolchain installed, as it is required to run the current state of the codebase.

```bash
rustup override set nightly
```

Below is a sample interaction combinator program that adds 2 and 3:

```
@main = x & 2 ~ $([+3] x)
```

It may be executed as follows

```plaintext
cargo run
Running `target/debug/ic.exe`
Please enter your code: @main = x & 2 ~ $([+3] x)

Tokens: [Reference, Alphanumeric("main"), Equal, Alphanumeric("x"), Ampersand, Alphanumeric("2"), Redex, Operate, LeftBracket, Operator(Add), Alphanumeric("3"), RightBracket, Alphanumeric("x"), RightParenthesis]

Program: Program {
    main: Book {
        name: Alphanumeric { s: "main" },
        net: Net {
            tree: Variable(Alphanumeric { s: "x" }),
            redexes: [
                Redex {
                    tree_1: Numeric(Number(Number { n: Alphanumeric { s: "2" }, native_num: U24(2) })),
                    tree_2: Operate(Operate { a: Numeric(Operation(Operation { op: Add, val: 3 })), b: Variable(Alphanumeric { s: "x" }) })
                }
            ]
        },
        others: []
    }
}

Result: Numeric(Number(Number { n: Alphanumeric { s: "5" }, native_num: U24(5) }))
```

### Project State

- The core functionality of the interaction combinator runtime is operational, but it needs optimization and refactoring.
- The `UnRust` compiler is still under development and not yet complete.
- I’ve intentionally avoided dependencies beyond the Rust standard library to maximize transparency, making this project highly self-contained.
