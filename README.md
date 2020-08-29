# grammlator20
Grammar Translator .Net Core Version 2020

Grammlator is a preprocessor accepting C# program files with special comments.
These special comments contain grammar rules with optional semantic attributes which look like C# formal method parameters.
The grammar rules define a control structure to call intermixed C# methods. 
The C# methods may have formal parameters which are associated by name to the semantic attributes.
Grammlator translates the grammar rules to readable performant C# code implementing a bottom-up analyzer
and inserts the generated C# instruction sequence at a well defined place in the source file.

Grammlator is not intended to be a compiler compiler. It "only" makes the bottom-up parsing algorithm available
as a control structure embedded into a C# program. 
It may be used to write lexers and parsers but also to write any other programs which analyze an input stream
with a structure defined by an suitable grammar.

What makes grammlator different to well known programs:

* well readable integration of the grammar rules as comments into a C# program
* generates readable performant c# code (no tables)
* implements bottom-up-analysis (by an explicitely programmed pushdown automaton or in simple cases a finite state machine)
* uses C# types for semantic attributes of terminal and nonterminal symbols
* may be used for small and large grammars e.g. to implement lexers and parsers
* may use its output as input (replacing the generated C# code)
* is not integrated in Visual Studio but it is easy to use it together with Visual Studio

State of the implementation of grammlator:
* the very first version "SKOP" had been used to implement a hardware design language in the years 1975 to 1980
* the further develoment is a one person project (just for fun)
* grammlator is used to implement its own lexer and parser
* There is a collection of simple samples
* As a more complex sample a C# lexer is in work (no, it is not intended to replace the Visual Studio C# lexer)
