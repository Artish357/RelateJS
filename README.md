# RelateJS
RelateJS is a relational interpreter JavaScript interpreter capable of synthesis powered by miniKanren.

# Paper abstract
We introduce a miniKanren relational interpreter for a subset of JavaScript, capable of synthesizing imperative, s-expression JavaScript code to solve small problems that even human programmers might find tricky. Specifically, we write a relational parser that parses s-expression JavaScript to a intermediate language called LambdaJS, and a relational interpreter for that language.  We show that program synthesis is feasible through the composition of these two disjoint relations for parsing and evaluation.

Created for miniKanren 2020 workshop

[Paper link](example.com)

## Requirements
* [Racket interpreter](https://racket-lang.org/) (Was tested on racket 7.4)

## Setup
To set up the repo and run tests, type the following lines into terminal:
```bash
git clone https://github.com/Artish357/RelateJS
cd RelateJS
raco test *tests.rkt
```
Look into [paper-tests.rkt](paper-tests.rkt) for examples.

## License
MIT
