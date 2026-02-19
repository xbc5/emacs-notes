# Emacs Notes

## Style

- Use small, single-purpose functions (aka atomic functions).
- Use composite functions to execute atomic functions. Prioritise readability: clear naming, logical call ordering, and clean layout.
- For functions: make file paths optional and default to `buffer-file-name`.

## Comments

- Create docstrings, and put each parameter on a separate line.
- In docstrings, escape quoted symbols with a backtick: `` `'() `` not `'()`.

Write comments...

- to explain conditional branches, with short, concise descriptions. I use comments to understand the code, so DO NOT skip this.
- as cause-and-effect: state the reason first, then the consequence with "so". Example: "The same ID might appear in multiple headings, so find which lists contain our items and update all of them."

## Rules

- Always consider third-party packages first. MELPA is a great resource; search for the packages on Google with "site:melpa.org".
- There is probably a function that does what you need. If not, only then should you write a fully custom solution. Eliminate third-party packages and provided built-ins first.
- Prefer `f-*` functions over built-in Emacs functions.

## Neutron

- Only top-level, composite functions should save changes. Atomic functions must never save buffers, because saving triggers auto-indexing.

## Smells

- If your solution is large and error-prone, you should find an existing solution instead.

## Org mode / Org-roam

### Session start

Once the session starts, and if you haven't already:

- Before writing any Org mode code, you MUST read the [documentation](https://orgmode.org/worg/doc.html) first.
- Before writing any Org-roam code, you MUST read the [documentation](https://www.orgroam.com/manual.html) first.

### Editing Org files

- Do NOT reinvent the wheel:
  - Create idiomatic solutions and NEVER make assumptions about how Org mode or Org-roam functions work. Always consult the documentation.
  - Use built-in Org mode and Org-roam functions for most things.
  - For complex, multi-line solutions, use the AST (Org Element API) instead.
  - For Org file edits, when the solution is 1-2 lines, use the correct, lightweight Org function for the job.
- If you use an AST:
  - NEVER parse a file into an AST more than once. Initialise it in the composite function, and inject it as a dependency, which should be an optional "ast" argument.
  - Read the [official documentation](https://orgmode.org/worg/dev/org-element-api.html) first.
  - Copy nodes when using local (partial) parsers, but when building a full parse tree, you can modify the AST in place.
  - Use the appropriate options to make rendering the AST as similar as possible to the source.
  - NEVER parse a file into an AST more than once. Accept an optional AST parameter where necessary, but always pass the AST down through function calls where possible.

### API

When we discover the most appropriate function for an action, I will ask you to record its signature in this section, with a concise description of what it does. For example:

```markdown
Always consider these functions when making the relevant changes:

- `(foo bar &optional baz`: Use this to create foo bar.
```

Always consider these functions when making the relevant changes:

- ...
