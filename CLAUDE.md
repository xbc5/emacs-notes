# Emacs Notes

## Style

- Use small, single-purpose functions (aka atomic functions).
- Use composite functions to execute atomic functions. Prioritise readability: clear naming, logical call ordering, and clean layout.
- Comment on conditionals, with short, concise descriptions.
- Create doc strings, and put each parameter on a separate line.

### Comments

Write comments as cause-and-effect: state the reason first, then the consequence with "so".

Example: "The same ID might appear in multiple headings, so find which lists contain our items and update all of them."

### File system

Prefer `f-*` functions over built-in Emacs functions.

## Org mode

### Before writing any Org mode code

**Read the documentation first.** Never make assumptions about how Org mode functions work. Consult https://orgmode.org/worg/doc.html before implementing any Org mode code. This must be done at the start of each session.

### Prefer builtins over custom logic

- Use built-in Org mode functions and Emacs APIs when available.
- Only write custom functions where built-ins do not suffice.
- If you find yourself writing custom logic, consider using the AST (Org Element API) instead.

### Modifying buffers

#### Use the Org Element API (AST)

##### Rules

- For more than minor changes, use an AST.
- If you need to use the API, read the [official documentation](https://orgmode.org/worg/dev/org-element-api.html) first. Project docs (CLAUDE.md, MEMORY.md) take precedence when they conflict.
- Copy nodes when using local (partial) parsers, but when building a full parse tree, you can modify the AST in place.
- Use the appropriate options to make rendering the AST as similar as possible to the source.
- If you must split the code into smaller functions, consider initialising the AST in the composite function, and injecting it as a dependency.
- Never parse a file into an AST more than once. Accept an optional AST parameter where necessary, but always pass the AST down through function calls where possible.
