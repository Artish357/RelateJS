# RelateJS
RelateJS is a proof-of-concept relational JavaScript interpreter capable of synthesis powered by miniKanren.
Currently, only a subset of JavaScript is supported, and programs have to be expressed in s-expression form.

## In short: what is relational programming? Why was it used?
Relational programming is a paradigm where programs are expressed through relations between data. Relational programs do not differentiate between input and output data, simply trying to fill "holes" within their relations. In the case of RelateJS, we can run the interpreter forward to execute JavaScript code or backwards to synthesize JavaScript source code that evaluates to a certain value, which can be very useful.

# Paper abstract
We introduce a miniKanren relational interpreter for a subset of
JavaScript, capable of synthesizing imperative, S-expression JavaScript
code to solve small problems that even human programmers might find tricky.
We write a relational parser that parses S-expression
JavaScript to a intermediate language called LambdaJS, and a relational
interpreter for LambdaJS.  We show that program synthesis is feasible
through the composition of these two disjoint relations for parsing and
evaluation.
Finally, we discuss three somewhat surprising performance characteristics
of composing these two relations.

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
