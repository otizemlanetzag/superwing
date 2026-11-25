# Superwing

Superwing is an independent browser-engine project. The JavaScript engine is written in Rust under JS/.

## JavaScript engine

The first interpreter layer includes tokenization, parsing, values, nested environments, variables, arithmetic, comparisons, conditionals, loops, function declarations, calls, and return values.

Run tests:

    cargo test --manifest-path JS/Cargo.toml

Run a script:

    cargo run --manifest-path JS/Cargo.toml -- example.js

This is an early foundation, not a complete ECMAScript implementation. Objects, arrays, classes, modules, exceptions, promises, async/await, and browser APIs are not implemented yet.
