# cse291d
Project for CSE 291D

* Use term rewriting to do stream fusion
* A complete pipeline should be 

    Program String --(parse)-->
    Syntax Tree --(typing)-->
    Typed Syntax Tree --(term rewriting)-->
    Typed Syntax Tree of Stream Program ---->
    String of Stream Program
  
  But currently this project focuses on Typed Syntax Tree -> String of Stream Program.
* Potential issues: hard to test this system, currently all test examples are hard-coded
* How to test the system:
  * create directory "outputs/"
  * run rewriter.hs
  * all output programs are generated in "outputs/"
  * all outputs are written in a single line, use a haskell formatter (like $hfmt -w) to make them look better
* TODO:
  * incorporate a front-end parser
  * implement typing system
  * implement more details (currently several branches are not implemented for time-saving purpose)
  * extend the language to allow pairs and etc
