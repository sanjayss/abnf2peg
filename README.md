# abnf2peg

This is an attempt to make a tool that can convert [Augmented Backus-Naur Form (ABNF)](https://en.wikipedia.org/wiki/Augmented_Backus%E2%80%93Naur_form) grammars to [Parsing Expression Grammar (PEG)](https://en.wikipedia.org/wiki/Parsing_expression_grammar) grammars.

This is written in the [D programming language](https://dlang.org/) and only converts to the PEG format understood by the [Pegged](https://github.com/PhilippeSigaud/Pegged) D language module. Not everything is converted.

ABNF grammars are used in a number of RFCs to define communication protocols and some file formats.

## How to build
To use, you need a [D language compiler](https://dlang.org/download.html) and the [Dub tool](https://github.com/dlang/dub) that is used to build and manage D language packages. Just do "dub build" from the root of the project to generate the *abnf2peg* binary for your platform.

## Caveats
  - ABNF _prose-val_ will need hand-editing
