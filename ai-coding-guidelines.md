# ai-coding-guidelines.md

This file provides guidance to AI coding tools (Claude Code, Copilot, Cursor) when working with code in this repository.

# Development Guide

Do not stage or commit your changes unless prompted to.

Always check that your changes can be built and pass the existing tests.

For a Haskell project using `stack`, this means the commands
`stack build` and `stack build --test` should terminate without errors.

## Project structure

This is a Haskell project using `stack`, which consists of different Haskell packages
in subdirectories.

The file `stack.yaml` configures the entire project and selects which resolver to use.
It also mentions the packages in subdirectories.

At the time of writing, only subdirectory `refactor-me` has a Haskell package setup.
The directories `log-parse` and `sink-node-graph` contain a description of a
software that will be added in development tasks.

## Build Command
```bash
stack build        # build all Haskell packages configured in stack.yaml
```

## Test Commands
```bash
stack build --test # test all Haskell packages configured in stack.yaml
```

## Development Guidelines
- Always verify changes build with `stack build`
- Format the code neatly. Consider using a formatting tool.
- Keep lines under 80 characters
- Don't add excessive comments unless prompted
- Don't disable warnings or existing tests unless prompted
- Use pattern-matching and functional programming idioms
- Avoid `error` calls and `undefined` and other unreachable code

## Important Notes

- ALWAYS avoid creating new files unless necessary
- ALWAYS prefer editing existing files
- NEVER proactively create documentation files (*.md) or README files
- NEVER stage or commit changes unless explicitly requested
