# cse291d
Project for CSE 291D

* Use term rewriting to do stream fusion
* A complete pipeline should be 

    Program String\\
     | (possibly desugar)\\
     | (parse)\\
     V\\
    Syntax Tree\\
     |\\
     | (typing)\\
     V\\
    Typed Syntax Tree\\ 
     |\\
     | (term rewriting)\\
     V\\
    Typed Syntax Tree of Stream Program\\
     | \\
     | \\
     V\\
    String of Stream Program\\
  
  But currently this project focuses on Typed Syntax Tree -> String of Stream Program.
* Potential issues: hard to test this system, currently all test examples are hard-coded
* TODO:
  * incorporate a front-end parser
  * implement typing system
  * implement more details (currently several branches are not implemented for time-saving purpose)
  * extend the language to allow pairs and etc
