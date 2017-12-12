# cse291d
Project for CSE 291D

Attempt: Component-based search

## General Procedure
- Expand sketch from type signature
  - Replace original lists with new streams
  - Generate stream state sketch for filling

- Enumerate components to fill holes
  - Composite components bottom-up
  - Simple syntax check for pruning

- Execute to partially verify the program
  - Arbitrarily generate stream with random skips
  - Execute both and compare their results

## Improvements
- Use haskell type checking system for pruning
- Enrich the sketch template for more general synthesis
