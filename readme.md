# lisp2js

lisp2js is a service that can convert programs in lisp into their javascript equivalent

## Live demo

  https://lisp2js.herokuapp.com/

## How to run

### To run the server

    yarn start

### To run only client part

    cd client
    yarn start

### To run both

    yarn dev
  
### To run mocha tests

    yarn test

## lisp2js API definition

### Endponts

  | Method     | Endpoint         | Usage                                                  | Passed data format        | Returned data format                                                 |
|--------    |--------------    |----------------------------------------------------    |-----------------------    |-----------------------------------------------------------------    |
| POST       | /isValidLisp     | Determine whether a string is a valid lisp program     | {   lispStr: string }     | {   status: string }                                                |
| POST       | /convertToJS     | Convert lisp program to Javascript                     | {   lispStr: string }     | {   status: string,   errorMessage: string,   codeJs: string  }     |


## Lisp syntax

### Three basic building blocks:

- atom
- list
- string
  
    atom: A number or string of contiguous characters. It includes numbers and special characters.
        list: A list of atoms and/or other lists enclosed in parentheses.
        string: A group of characters enclosed in double quotation marks.

### Constants

    t - true
    nil - null

### Variables

    (defvar var value) - global variable declaration

    (setq var value) - variable asignment. If variable is not declared yet then setq declares 
    local variable

### Operators

#### Arithmetical operators

      +
      -
      /
      *

#### Comparison operators

      <
      >
      <=
      >=
      =
      /=

#### Boolean operators

      and
      or
      not

### Functions

    (defun function-name (par1, par2, ...)
      (body-expression 1)
      (body-expression 2)
      ...
      (body-expression )
    )

    defun returns the value of the last expression

### Branch operators

    (cond (test1    action1.1 action1.2 ...)
          (test2    action2.1 action2.1 ...)
          ...
          (testn   actionn.1 actionn.1 ...)
    )

    (if (test-clause) (action-if-true) (action-if-false))

    (when (test-clause) action1 action2 ...)
  
### Loops

    (loop for loop-variable in <a list>
       do (action)
    )

    (loop for loop-variable from value1 to value2
      do (action)
    )

### Built In Functions

    write: transforms to process.stdout.write()
    write-line: transforms to console.log()
    min: returns minimun from parameter list
    max: returns maximum value from parameter list
    car: returns the first element of a list
    cdr: returns the rest of a list except the first element

## Capabilities & limitations

### Capabilities

    1. The lisp2js converter implements a part of Common Lisp syntax. 
        Syntax definitions I took from this page: https://www.tutorialspoint.com/lisp/index.htm
    2. lisp2js could check if the syntax of given lisp program is correct.  
  
### Limitations

    1. lisp2js is case sensitive. It means that you, for example, cannot use DEFVAR instead of defvar. 
    2. It implements a very few built-in lisp functions.

## Problems and solutions

1. Problem: I have not found any suitable Lisp to Javascript converter on the Internet.
  Solution: I decided to write Lisp parser in form of a state machine with stack memory.
  Here is a State Transition Diagram I developed for the state machine:
  https://docs.google.com/spreadsheets/d/1D6l0KnaemCNLSWCuSUvlljfsmhA45UiMhDwQByot9xU/edit?usp=sharing

2. Problem: The result of lisp parser work is a list of lists. 
Each list consists of atoms, strings, lists, and comments. How to generate Javascript from this data structure?
  Solution: I decided to convert the list of lists onto scopes tree and generate JS in parallel. Each scope consists of variables, functions, and body.

3. Problem: I found out that it is very difficult to convert comments properly. I decided to store comments to buffer and output them only when a new line event occurs in the result string.

4. Problem: Conditional operator "cond" in lisp returns value. How to convert this operator into Javascript?
  Solution: In case of this operator is used inside of expression I wrap it all into arrow function with the immediate call of it which returns the result of the last clause execution of the corresponding block. 

For eaample:

    (setq v3 
      (cond (t 
              (write-line "this is cond #2")
              (+ 1 2)
            ) 
      )
    )

 is converted into:

    let v3 = (_ => {
      if (true) {
        console.log('this is cond #2');
        return 1 + 2;
      } else return null;
    })();

5. Problem: How to make output code look better?
  Solution: I use the js-beautify package to prettify js code.
