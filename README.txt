my-compiler-c64

This project contains my compiler and compiler-compiler experiments from 1990, for C64 and C128, in BASIC.
These compilers use simple algorithms to perform both lexical and syntactic analysis of simple languages.

I later went on to take compiler classes towards a master's degree and enjoyed them immensely.
Had an opportunity to get a job building compilers, but blew it.

-----

*.cbmprj file is for CBM prg Studio
*.bas files are modern ASCII versions of Commodore BASIC programs that CPS compiles to proper tokenized files.

Compiled *.prg should be written to disk as PRG type, without extension.
*.seq likewise should be written as SEQ type, without extension.

== for C-64 ==

rekanizer.bas is actually an auto-complete for a fictitious advanced BASIC.
parser.bas is a simple var=expression parser
  - was renamed to parsec.bas
  - became a var=expression compiler

== for C-128 ==

compiler.bas is compiler for the FIG (friendly, interactive, graphic) language
gencom "generic compiler language" is unknown
cfg is a ll(1) parser generator
