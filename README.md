# Lisp_Interpreters
1. **Recursion and High-order Function**

   This piece of code lists a series of classic recursive LISP functions for list processing such as: map, reduce, filter and so on.

2. **Lexical Address**

   This piece of code contains a few small interpreters that take “code” as input and generate analyses such as whether or not a variable is free or bounded in a function.

3. **Interpreter**

   The first complete interpreter that take "code" and generate execution value. The environment, the context holding local variable values, can be modeled by using either a function or  a table data structure.

4. **Dynamic Scope Interpreter**

   An interpreter that interpret input code in a dynamic scope manner.

5. **Parameter Passing Style**

   The semantics of different language is different. For some programming language, the parameters passed to functions are reference. Some other languages pass parameter by value.  

   This piece of code contains interpreters that interpret code assuming four different parameter passing styles: call-by-value, call-by-reference, call-by-name, and call-by-need. 

6. **Continuation Passing Style**

   Continuation passing style allow functional programming languages to explicitly control their execution flows. This piece of code shows how to write functional program in CPS style.

7. **CPS Interpreter**

   The interpreter is rewritten in a CPS style in this code piece.