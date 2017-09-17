=====
Lexyn
=====

Description
===========

**Lexyn** is a simple expression analyzer which checks mathematical expression
for validity. This is also a part of my `High-Performant systems software.
Compilers course`_ assignment.

Currently the **Lexyn** functionality is very limited. Everything Lexyn can is
to tokenize the input string and perform a basic syntax analysis. In future it
will be able to build a parallel execution plans for given expression and
determine the best of them.

Usage
=====

.. code-block:: bash
   
   $ Lexyn '2 + 2'
   $ Lexyn '2++2'
     Unexpected token `+` at position 3
     2++2
       ^
   $ Lexyn '2@@2'
     Unexpected symbol '@' at position 2
     2@@2
      ^
     Unexpected symbol '@' at position 3
     2@@2
       ^
   $ Lexyn '22 +'
     Unexpected end of input at position 4
     22 +
        ^
   $ Lexyn '2*22 + () - 1'
     Empty parens are not allowed at position 9
     2*22 + () - 1
             ^

Building
========

`Haskell stack`_ is used for project development. To build the project just
execute ``stack build`` in the project directory.


.. _`High-Performant systems software. Compilers course`: https://kpi-fict-ip32.github.io/Blog/s09/compilers.html
.. _`Haskell stack`: https://www.haskellstack.org/
