Touch
=====

This is a concatenative programming language for programming on small touch devices such as smart phones.


Concatenative programming
-------------------------

In a concatenative programming language, juxtaposition `f g` means function composition *g ○ f*, 
and all functions take a stack as their input and produces a stack as their output.

It's remniscent of the pipe operator in F# `|>`, but is generalized to piping any number of values around.

The type of addition `+` in a concatenative language may look like this: `S* Number Number -> S* Number`, 
eg. it's a function that takes two Numbers and returns one Number. The rest of the stack `S*` is returned as-is.
In other words, it takes a stack with two Numbers on top and returns a stack with one Number on top.

A function can naturally consume *and produce* multiple values. 
For example, the `swap` function swaps the two top values of the stack and thus has type: `S* a b -> S* b a`.

Since most functions only manipulate the topmost values of the stack, we can use the notation `a b -> b a` instead, which expands to the former type.
