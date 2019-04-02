# my-language

## BNF-style grammar

Expression ::= \
  | Number\
  | Expression  +   Expression\
  | Expression  -   Expression\
  | Expression  *   Expression\
  | Expression  /   Expression\
  | ( Expression )

Number ::= \
  | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | NumberNumber
  
  ## Example
  
  ( 0 + 1 ) * ( 4 / 2 ) - 1
  
  Spacing is part of the syntax currently
