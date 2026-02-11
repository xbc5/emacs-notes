# Emacs Notes

## Style

- Use small, single-purpose functions (aka atomic functions).
- Use composite functions to execute atomic functions. Prioritise readability: clear naming, logical call ordering, and clean layout.
- Comment on conditionals, with short, concise descriptions.
- Create doc strings, and put each parameter on a separate line.

## Org mode

### Modifying buffers

#### Use the Org Element API (AST)

##### Rules

- For more than minor changes, use an AST.
- If you need to use the API, read the [official documentation](https://orgmode.org/worg/dev/org-element-api.html) first. Project docs (CLAUDE.md, MEMORY.md) take precedence when they conflict.
- Copy nodes when using local (partial) parsers, but when building a full parse tree, you can modify the AST in place.
- Use the appropriate options to make rendering the AST as similar as possible to the source.
- If you must split the code into smaller functions, consider initialising the AST in the composite function, and injecting it as a dependency.
