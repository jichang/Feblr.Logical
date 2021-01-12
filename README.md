# About this project

This project aim to implement a programming language like Prolog. I have no intend to make it production ready.

# Syntax

## Most of the syntax should be similar to Prolog, there's only one data type: term, it consists 6 kinds of data:

1. Atom are characters start by lowercase letter, like

    ```
    atom
    another
    ```

2. Variable are characters start by uppercase letter, like

    ```
    VarA
    VarB
    ```

3. Numbers

    ```
    10000
    11110.9
    ```

4. Strings are escaped by *`*

    ```
    `this is a string`
    ```

5. List are sequences surrounded by []

    ```
    [atom, VarA, 10000]
    ```

6. Compound terms looks like functions, its format is `functor(...arguments)`

    ```
    hello(world, X, "Hello, world")
    ```

## Program are constructed by clauses. There are two kinds of clauses

1. Fact

    ```
    fact.
    fact(hello, X).
    ```

2. Rule

    ```
    grandfather(X, Y) :- father(X, Z), father(Z, Y).
    ```
