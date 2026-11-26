# Superwing

Superwing is an independent Jד-engine project written in Rust under `JS/`.

## Current interpreter features

- Variables, nested scopes, arithmetic, comparisons, strings, booleans, and nullish values.
- Conditional statements, `while` and `for` loops.
- Named functions, parameters, and return values.
- Array and object literals, indexed access, property access, and property assignment.
- Array `push()` / `pop()`, and `length` for arrays and strings.
- `typeof` and prefix/postfix increment/decrement for simple variables.
- Logical `&&` / `||` with short-circuit evaluation, plus strict equality `===` / `!==`.
- `break` and `continue` in `while` and `for` loops.
- `do...while`, the conditional (ternary) operator, and numeric remainder (`%`).
- Compound assignment operators: `+=`, `-=`, `*=`, `/=`, and `%=` for variables.

Run tests:

    cargo test --manifest-path JS/Cargo.toml

Run a script:

    cargo run --manifest-path JS/Cargo.toml -- example.js

This is an experimental interpreter, not a complete or standards-compliant ECMAScript implementation. Major missing areas include correct lexical declaration semantics, prototypes, constructors, classes, exceptions, modules, closures with full JavaScript semantics, promises, async/await, and browser APIs.
