## part a
Python's grammar is defined in `Grammar/python.gram`. To add the until statement, modify the grammar to include a new rule similar to while_stmt:
```ebnf
compound_stmt:
    ...
    | while_stmt
    | until_stmt

until_stmt:
    | 'until' test ':' suite ['else' ':' suite]
```

## part b
The AST structure is defined in Parser/Python.asdl. To add until, define a new node:
```asdl
stmt = ... | Until(test test, stmt* body, stmt* orelse)
```
The parse tree is converted to an AST using `Parser/pegen/parse.c`. The AST generation uses the existing Python interpreter (ast.py and symbol.py) for bootstrapping.

## part c
Instead of writing new C code, translate until to a while loop with an inverted condition. For example:
```python
until condition:
    # body
```
is equivalent to:
```pythonwhile not condition:
    # body
```