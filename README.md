# abnf2peg
Convert ABNF grammars to PEG grammars (sort of)

This is an attempt to make a tool that can convert [Augmented Backus-Naur Form (ABNF)](https://en.wikipedia.org/wiki/Augmented_Backus%E2%80%93Naur_form) grammars to [Parsing Expression Grammar (PEG)](https://en.wikipedia.org/wiki/Parsing_expression_grammar) grammars.

This is written in the D programming language and only converts to the PEG format understood by the [Pegged](https://github.com/PhilippeSigaud/Pegged) D language module. Not everything is converted.

ABNF grammars are used in a number of RFCs to define communication protocols and some file formats.
