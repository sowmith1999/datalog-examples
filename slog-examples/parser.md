# Notes for the parser

- Every statement at the top is either a fact or a rule.
- There are only two base values, string and a number, uses string? and number? 


### Questions
- What does it mean for a datalog rule/fact to be fully ground
    - How are they checking in the `fact?` function, if the input is fully ground
- In the below match expression, does the fact-or-base? get run on every list element matched by the `...`    
```racket
[`(,(? symbol? rel) ,(? fact-or-base? args) ...) `(,rel ,@args)]
```
- What is syntax or way to EBNF grammars?, Everything seems to described in terms of writing a EBNF grammar


#### Racket questions
- What is `with-handlers` in racket?
- 


