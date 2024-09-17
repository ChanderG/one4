# one-four

A forth inspired language built on Fennel.

## Syntax

We have 2 primary data stores:
1. The stack - to use to pass in arguments to words.
2. The store - a map of words to values/definitions.

Words are the fundamental unit - representing variables and functions.

### Variables

To define variables:
```
age var
```
defines a variable named "name".

To set the variable:
```
18 age !
```

To use the variable (ie, push the value of the variable onto the stack):
```
> age ?
> .s
{18}
> 10 age ? +
> .s
{28}
```

Notice how this definition is different from the Forth one - Postfix notation instead of prefix. This is done out of laziness - out of the idea of having the least possible number of special forms in the eval.

### Functions

New words are defined with `:` and `;` like in Forth, with one small difference - the word name goes before!

```
square : dup * ;
```
