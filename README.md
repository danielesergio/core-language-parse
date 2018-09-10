# Core Language Parser
Project for the `Funcntional Languages` course of University of Paudua's Master Degree in Conputer Science.

The project consinst into realize a parser of the `Core Language` using `Haskell`
 
## Core Language Grammar
<pre>
Programs             program -> sc<sub>1</sub>; ...; sc<sub>n</sub>                 n>=1

Supercombinators          sc -> var<sub>1</sub>; ... var<sub>n</sub>                n>=0

Expressions             expr -> <b>let</b> defns <b>in</b> expr
                             | <b>letrec</b> defns <b>in</b> expr
                             | <b>case</b> expr <b>of</b> alts
                             | var<sub>1</sub> ... var<sub>n</sub> . expr
                             | expr1
                           
                       expr1 -> expr2 <b>|</b> expr1
                              | expr2

                       expr2 -> expr3 & expr2
                              | expr3

                       expr3 -> expr4 relop expr4
                              | expr4

                       expr4 -> expr5 + expr4
                              | expr5 - expr5
                              | expr5

                       expr5 -> expr6 * expr5
                              | expr6 / expr6
                              | expr6
                      
                       expr6 -> aexpr<sub>1</sub> ... aexpr<sub>n</sub>              n>=1
                      
                       aexpr -> var
                             | num
                             | <b>Pack</b>{num,num}
                             | (expr)
                            
Definitions            defns -> defn<sub>1</sub>; ...; defn<sub>n</sub>              n>=1
                        defn -> var = expr
                      
Alternatives            alts -> alt<sub>1</sub>; ...; alt<sub>n</sub>                n>=1
                         alt -> <num> var<sub>1</sub> ... var<sub>n</sub> -> expr         n>=0
                        
Binary operators       binop -> arithop | relop | boolop
                     arithop -> + | - | * | /
                       relop -> < | <= | == | ~= | >= | >
                      boolop -> & |<b>|</b>
                     
variables                var -> alpha varch<sub>1</sub> ... varch<sub>n</sub>        n>=0
                       alpha -> an alphabetic character
                       varch -> alpha | digit | _
                      
Numbers                 num  -> digit<sub>1</sub> ... digit<sub>n</sub>              n>=1

</pre>

## Operators priority and associativity

 Precedence     | Associativity | Operator          |
| :-----------: |:-------------:| :-----------------|
| 6             | Left          | Applcation        |
| 5             | Right         | *                 |
|               | None          | /                 |
| 4             | Right         | +                 |
|               | None          | -                 |
| 3             | None          | == ~= > >= < <=   |
| 2             | Right         | &                 |
| 1             | Right         | \|                |

# Commands
The project is realized using the [Haskell tool stack](https://docs.haskellstack.org/en/stable/README/) 

These are the main commnads:
<pre>

Build project with: <i>stack build</i>

Run test with: <i>stack test</i>

Start project with: <i>stack build --exec core-language-parser-exe</i>

Start project with: <i>stack exec core-language-parser-exe -- &lt PATH-FILE-TO-PARSE &gt </i>

Default value of &lt PATH-FILE-TO-PARSE &gt is "res/input.txt"
</pre>