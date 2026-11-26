# Superwing

Superwing is an independent browser-engine project. The JavaScript engine is written in Rust under `JS/`.

## JavaScript engine

The current interpreter foundation includes tokenization, parsing, values, nested environments, variables, arithmetic, comparisons, conditionals, loops, function declarations and calls, arrays, indexed array access and assignment, object literals, object property access and assignment, and basic array `push()` / `pop()` methods.

Run tests:

    cargo test --manifest-path JS/Cargo.toml

Run a script:

    cargo run --manifest-path JS/Cargo.toml -- example.js

This is still an early interpreter, not a complete ECMAScript implementation. Important missing features include standards-compliant coercion and equality, lexical declaration semantics (`const` / `let` / `var`), prototypes, constructors, classes, exceptions, modules, promises, async/await, and browser APIs.
