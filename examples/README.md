# Examples

This folder contains examples demonstrating how to use `treestand`.

## How to run an example

1.  Navigate to the example directory (e.g., `01_basic`).
2.  Run the generator script to create the parser:
    ```bash
    nim r gen.nim
    ```
3.  Run the main application:
    ```bash
    nim r main.nim
    ```

## Available Examples

*   **01_basic**: Define a simple grammar, generate a parser, parse a string, and walk the tree.
*   **02_edit**: Demonstrate how to use the `edit` API to update tree positions after a source change.
*   **03_query**: Demonstrate the `Query` API (stub) for pattern matching.
