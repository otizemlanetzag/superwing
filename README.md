# Superwing

Superwing is an independent browser-engine project. Its experimental JavaScript interpreter is written in Rust under `JS/`.

## Current interpreter features

- Numbers, strings, booleans, `null`, and `undefined`.
- Variables, nested scopes, and named functions with parameters and return values.
- Arithmetic: `+`, `-`, `*`, `/`, and `%`.
- Comparisons, strict equality (`===` / `!==`), and logical short-circuit operators (`&&` / `||`).
- Compound assignment: `+=`, `-=`, `*=`, `/=`, and `%=` for variables.
- Conditional statements, the ternary operator, `while`, `do...while`, and `for` loops.
- `break` and `continue` in loops.
- Array and object literals, indexed access, property access, and property assignment.
- Array `push()` / `pop()`, and `length` for arrays and strings.
- `typeof` and prefix/postfix increment/decrement for simple variables.

Run tests:

    cargo test --manifest-path JS/Cargo.toml

Run a script:

    cargo run --manifest-path JS/Cargo.toml -- example.js

This is an experimental interpreter, not a complete or standards-compliant ECMAScript implementation. Major missing areas include correct lexical declaration semantics, prototypes, constructors, classes, exceptions, modules, closures with full JavaScript semantics, promises, async/await, and browser APIs.
