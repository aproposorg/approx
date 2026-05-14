---
name: approx-paper-to-chisel
user-invocable: true
description: "Use when reading a scientific paper about approximate arithmetic and implementing the design in Chisel. Guides the agent to extract the architecture, map paper cell types to Chisel, and keep the implementation hardware-accurate and idiomatic."
tool-allowances:
  - sbt
  - pdftotext
  - pdfimages
---

# Approximate Arithmetic Paper to Chisel Skill

## Use when

- the task is to read and implement an approximate arithmetic unit described in a scientific paper
- the implementation should be in Chisel and must reflect paper terminology and architecture clearly

## What to do

1. Identify the exact architecture described by the paper.
   - map paper symbols and cell names directly into code as much as possible
   - preserve the distinction between exact and approximate cells
   - preserve transition boundaries where carry behavior changes

2. Extract hardware structure first.
   - for adders, identify the "style" of addition (e.g., ripple-carry, carry-lookahead, etc.) and how approximation is applied
   - for multipliers, identify partial product generation by column and row, cell-level behavior for each column type, and recursive or hierarchical decomposition if the paper uses chunks or sub-multipliers
   - for other units, maintain the structure of the design as described

3. Implement explicit Chisel cell units.
   - define objects/classes for each paper cell type
   - use clear method signatures and comments matching the paper
   - make the Chisel code look like the paper architecture, not a hidden maths rewrite

4. Prefer Chisel hardware constructs.
   - use `Wire`, `Vec`, and explicit hardware-level aggregations
   - prefer explicit concatenation with `##` (including for shifts) over `Cat`
   - avoid Scala collections for hardware state when possible, or default to `Seq` or `IndexedSeq`
   - avoid the use of scala `var` to hold hardware state or signals

5. Handle non-ideal widths and operand signs.
   - if input width is not a natural multiple of the design block size, pad operands cleanly
   - keep the padded logic explicit and trim the final output
   - if a design is for signed/unsigned operands only, note it clearly

6. Preserve validation feedback.
   - do not implement verification code for approximate designs, instead only implement an object to generate the design for different parameters
   - rely on or extend existing verification infrastructure to test exact designs

## Do

- if the paper names specific cell types, use exactly those names in the code and comments
- model the exact transition from approximate to exact columns with the appropriate cells
- if the design is hierarchical, make the chunk/split structure visible in the code
- keep the final output width trimmed to the original size after padding

## Do not

- do not replace the paper’s cell-level descriptions with a generic `PopCount` or arbitrary bitcount hack unless the paper says so

## Example intent

- "Implement the carry-disregard multiplier from the paper in Chisel, with explicit Cell50/Cell51/Cell52/Cell53 behavior, and hierarchical chunking for larger widths."
- "Translate the paper’s hardware algorithm into a Chisel design that reads like the original architecture and avoids hidden software-style tricks."
