System Description
==================
We have implemented a programable calculator that has capabilities in multiple
computer science related concepts a first or second year Cornell student my need
help with. These topics include modular arithmetic, linear algebra,
combinatorics, as well as basic calculator functionality such as multiplication
and addition.

Discrete Help
-------------
Functionality inculding :
- Computing powers modulo a number
- GCD
- LCM
- factorization (of reasonable numbers)
- computation of the totient (of reasonable numbers)
- RSA Cryptography:
  + encryption given public key
  + decryption given a private key
  + small prime generation
  + probabilistic large prime generation
- Solving systems of linear congruences modulo a number (and other easy
diophantine equations, in addition to indicating if simple quadratic
equations have a solution)
- Basic Combinatorics including factorials, permutations, and combinations

Linear Algebra Help
-------------------
Simple matrix operations
- adding
- scaling
- dot product
- cross product
Operations for system of linear equations including
- transforming a matrix to row (reduced) echelon form
- solving a system of linear equations
- calculating the inverse using row operations Calculating Determinants
- Operations on Vector spaces including indicating linear independence and
dependence and finding the basis of the col/null space of a matrix

Programmability
---------------
Our programming language is a small mathematical stack based language written
in postfix (RPN) notation. The calculator comes with many built in operators
for all of the above outlined topics, and allow the user to write their own
operators

Dependencies
============
The full project depends on the folowing packages, but not all parts of the
project will require all of them to run
* oUnit
* str
* nums
* yojson
* cohttp
* lwt
* ocurl

Instructions for set up
========================
+ Running the Calculator in the terminal
  - commands will be case sensitive
  1. The only the packages used with the command line version are oUint, nums, and str
  so you should not need to install anything to run the command line version
  2. There is a make file to make running the code simpler
    - required packages are oUnit(see #1 for install command)
    and Nums(does not require install)
    - `make test` will run the test suite
    - `make repl` will compile and run the repl, (control-c will quit)
    - `make compile` will compile all the non server source code
    - `make clean` will clean up the files built
+ Running the server
    - this server is partially adapted from [ohttp](https://github.com/RamV13/ohttp/blob/master/README.md)
    - This will require installing some extra packages:
       * oUnit, str, nums, yojson, cohttp, lwt, ocurl
    - to install these dependencies, run:
        `make install`
    - `make server` will compile and run the server

+ Using the messanger bot
  * The instance we set up is in testing mode, since facebook does not allow
  them to be public, access must be explicitly given, ztb5 can give you access
  * To set up your own instance:
    - to connect bot to facebook, visit https://developers.facebook.com/docs/messenger-platform/getting-started/quick-start
    and follow the tutorial
    - once bot is connected to facebook, you can start a conversation by messaging the page the bot is linked to
    - these commands will be case insensitive
    - multiple users are suported, and their enviroments will be strored seperately

Using the calculator
================
* note: if an error is raised and you do not interact with the resulting error
that error may be burried on the stack, for example `2 0 / 3 5 *` will return
`15` and will not display the divide by zero error, when the following says
.... displays the error this is suceptible to happen

Types
------
+ integers (of infinite size)
  - ex. `35738589746878732509813958093850980935`
+ floats (of 64 bit size)
  - ex. `34.564`
+ strings (must be contained in quotation marks)
  - ex.   `"Hello!!"`
+ matricies (row major) and require spaces after commas and no spaces after ]
  - ex. `[[2., -3.], [-4., 5.]]` is the matrix
  | 2. -3. |
  | -4  5. |

Built in Operators
------------------
simple arithmatic
*****************
+ `a b +` will compute the normal integer/floating point/matrix addition of
`a` and `b`
  where `a` and `b` are of the same types, if they are both matricies then they
  must be of the same size
+ `a b -` will compute normal integer/floating point subtraction `a` and `b`
  where `a` and `b` are of the same types, if they are both matricies then they
  must be of the same size
+ `a b *` will compute normal integer/floating point multiplication of `a` and `b`
+ `a b /` will compute integer/floating point division of `a` `b`
  - note: integer division will be floored for numbers where the `b` does not
  divide the `a`. If `b` is 0 then this equation will evaluate to an error,
  `"division by zero"`.
+ `a b ^` operator will raise `a` to the `b` power where `a` and `b` are normal
integers or floating point numbers. This may cause floating point numbers to
overflow, but integers
will never overflow their value, though could cause a slow response.
+ `a b =` tests equality of `a` and `b` which are normal integer, floating
point, or matrix values
+ `a b c ?` will be used like an if statement. `a` will act as the guard, so if
it evaluates to a non zero value then the stack will process `b` and if `a` is
a zero value then `c` will be processed by the stack.


modular arithmetic
******************
+ `a b %` will compute the remainder of `a` when divided by `b` where `a` and
`b` are integers, and `b` is positive if b is 0 then the error message "division
by 0" will be displayed
+ `a b c +~` will compute the sum of `a` and `b` taken mod `c` where `a`, `b`,
and `c` are integers, and `c` is positive
otherwise the error message "cannot take the remainder mod a non-positive number"
will be displayed
+ `a b c -~`  will compute the difference of `a` and `b` taken mod `c` where
`a`, `b`, and `c` are integers, and `c` is positive otherwise the error message
"cannot take the remainder mod a non-positive number" will be displayed
+ `a b c *~` will compute the product of `a` and `b` taken mod `c` where `a`,
`b`, and `c` are integers, and `c` is positive
otherwise the error message "cannot take the remainder mod a non-positive number"
will be displayed
+ `a b c /~` will compute the division of `a` and `b` taken mod `c` where `a`, `b`,
and `c` are integers, and `c` is positive
otherwise the error message "cannot take the remainder mod a non-positive number"
will be displayed
  - note: if the `b` is not relatively prime to `c` then division cannot proceed
  and the error `"second arguement is not relatively prime to divisor"` will be
  displayed.
+ `a b c ^~` operator will raise `a` to the `b` power then take the remainder
mod `c`, where `a`, `b`, and `c` are integers, and `c` is positive otherwise
the error message "cannot
take the remainder mod a non-positive number" will be displayed
+ `a b c =~` tests the equality of `a` and `b` mod `c`, where `a`, `b`,
and `c` are integers

other modular operators
***********************
+ `a b gcd` computes the greatest common divisor of `a` and `b`, where `a`, `b`,
and `c` are integers
+ `a b lcm` computes the least common multiple of `a` and `b`, where `a`, `b`,
and `c` are integers
+ `a factor` computes the factors of `a`, where `a` is an integer dispaying them
as a list with their multiplicities
+ `l gen_prime` is an integer representable with `l` bits, which is prime with
very high probability and gives the error message "no primes this small" if the `l`
is to small
  - ` 'prime` will give the most recently generated prime
+ `n is_prime` is 1 if `n` is prime and 0 if `n` is not prime, can be slow
for large `n` (for large n it could take lifetimes to finish, control-c will
get you out of this sticky situation)
+ `n is_prime_prob` is 1 if `n` is prime and 0 if `n` is not prime with high
probability, much faster than is_prime, but not guaranteed to be correct,
roughly probability 1/2^100 of n being composite.
  - note: if the user types `'prime_prob` you will receive the result of the
  last call to is_prime_prob
+ `a totient` computes the Euler’s totient function of `a` where `a` is an
integer > 0 otherwise error message "totient undefined for non_positive values"
+ `p q d public_key` will generate a public key based on the private key
`(p, q, d)`
  - `'n` and `'e` will give the corresponding components of the public key most
  recently calculated

  - though public_key takes 3 parameters, it can take the resulting
  tuple produced by gen_priv_key
+ `gen_priv_key` will generate, with very high probability, a triple
(d,p,q) where p,q are two large prime numbers, and d is a unit modulo p*q
   - `'p`, `'q`, and `'d` will give the corresponding components of the
   private key most recently generated
+ `msg n e encrypt` will encrypt the string `msg` using `n` `e` as the public
key where `n` is a large product of two distinct, and `e` is a unit modulo `n`.
  - though encrypt takes 3 parameters, it can take 2 with the second being the
  resulting pair created by `public_key`
+ `c d p q decrypt` will decrypt the number `c` using `d` `p` `q` as the
private key where `p` is a large prime integer, `q` is a large prime integer,
and `d` is a unit modulo the product of `p` and `q`.
  - though encrypt takes 4 parameters, it can take 2 with the second being the
  resulting tuple produced by gen_priv_key
+ `c n e crack` will attempt to decrypt the integer `c` using `n` `e` as the
public key where `n` is the product of 2 large primes, and `e` is a unit mod `n`.
  - though crack takes 3 parameters, it can take 2 the second being the resulting
  pair created by `public_key`
+ `a b c bezout` is a pair (x, y) where x*a + y*b = c, or an exception message
will be displayed if no such value exists
+ `b0a0 … bnan n solve` solves the system of equations x=a₀ (mod b₀) … x=an(mod bn)
where the as and bs are integers, there are `n` of those equations (otherwise
a parsing error is enountered), the b's are relative prime (otherwise the method
may fail, or give a false result), and each b is positive (otherwise the
error message `"cannot take the remainder mod a non-positive number"` will be
displayed)
+ `a p square` indicates if a is a square mod p where p is a positive prime,
and a is an integer. Gives the exception value "cannot take the remainder mod
a non-positive number", if p <= 0.

combinatroics
*************
+ `n !` is the calculation of a factorial where `a` is a positive integer
+ `n k choose` is the calculation of `n` choose `k` where `n` and `k` are
positive integer
+ `n k perm` is the calculation of the number of choices of `n` items
from `k` where order matters, `n` and `k` are positive integers
+ `a b part` is the calculation of the number of ways `n` items
can be partitioned into `k` categories where all `n` items would be
indistinguishable, `n` and `k` are positive integers

linear alg
**********
* note: in these calculations if integer values are given then computation
will proceed in the rational numbers, to avoid errors from tructation in integer
division any time the improper sizes of imput matricies an erroe message
"matrix size issue" will be diplayed
+ `v1 v2 .` computes the dot product of `v1` and `v2` where `v1` and `v2` are
column vectors
+ `v1 v2 #` computes the cross product of `v1` and `v2` where `v1` and `v2` are
column
vectors of height 3
+ `m s *` computes `m` scaled by `s` where `s` is a number and `m` is a matrix
+ `m n col` gives the `n`th column of the matrix `m` where `n` is a 64 bit
integer with
within the matrix `m`
+ `m n row` gives the `n`th row of the matrix `m` where `n` is a 64 bit integer
within the matrix `m`
+ `m inv` computes the inverse of the matrix `m` if `m` is invertable
+ `m transpose` computes the transpose of the matrix `m`
+ `m echelon` reduces `m` to echelon form where `m` is a matrix
+ `m reduce` reduces `m` to reduced echelon form where `m` is a matrix
+ `m b matrix_solve` solves the linear systems of equations `m`x=`b` where `m`
and `b` have the same number of columns. if the system is inconsitent then the
error message "inconsistant system" is displayed
+ `m det` computes the determinant of `m`, where `m` is a square matrix
+ `a indep` computes if the columns of `a` are linearly independent where `a`
is a matrix
+ `m rank` computes the rank of the matrix `m`
+ `m dep` computes if the columns of `m` are linearly dependent where `m`
is a matrix
+ `m indep` computes if the columns of `m` are linearly independent where `m`
is a matrix
+ `m nullspace` computes a matrix with the columns of the matrix being the null
space of `m` where `m` is a matrix
+ `a colspace` computes a matrix with the columns of the matrix being the column
space of `m` where `m` is a matrix

Defining a New Operator
-----------------------

A function defintion must be entered in its own message to the calculator and be
in the following form
  `{<name of function>: <param₁> … <paramn> -> <stack program for method>}`

Then to execute the function the user can list the arguments and then place the
function name after the arguments listed in order
* ex. ```
      { add1 : x -> x 1 + }
      ```
      ```
       3 add1
       ```
- note₀: we do not have constant variable definitions, but if you want
to define a variable they could create a constant function that will always
evaluate to some value.
    * ex. ```
          {x :-> 5}
          ```
          ```
           x
           ```
- note₁: we will be using lexical scope for operators.
