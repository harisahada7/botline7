open Array
open Big_int

(* AF: we an integer is represented as a infinte sized integer
 * RI: none *)
type integer = Big_int.big_int

(* AF: the numbers in our launguage include integers, 64 bit floating point
 * numbers, and rational numbers where the pair (a,b) represents the
 * value a/b
 * RI: none
 *)
type number = I of integer | F of float | Q of integer * integer

(* AF: if some error occured in computation the value will be the exn
 * where the exn is a string describing the error
 * RI: none
 *)
type exn = string

(*AF: a matrix will be represened as an array of arrays and may contain
 * any of the values. The matrix:
 * | a1 b1 c1 ... |
 * | a2 b2 c2 ... |
 * | .            |
 * |  .           |
 * |   .          |
 * will be represented as [|[|a1;b1;c1 ... |]; |a2;b2;c2 ... |]; ... |]
 * note: that matricies are row major in this case c2 is the 2,3 entry
 * RI: all of the rows in the matix are of the same length and all of
 * the entries in the matix are from the same number constructor
 *)
type matrix = number array array

(* AF: A public_key of (n,e) is an RSA public key
 * where gcd(e,n) = 1 and n = pq for some
 *  primes p and q
 * RI: n is a product of two primes and e is a unit mod [n]
 *)
type public_key = integer * integer

(* AF: A private_key of (d,p,q) is an RSA private key (d,pq) for
 * the public key (e,n), where gcd(d,n) = 1
 * and d satisfies e*d = 1 (mod phi(n)), where phi
 * is the Euler phi function
 * RI: p and q are primes and d is a unit mod the product of
 * p and q*)
type private_key = integer * integer * integer

(*AF: the factors of a number are represented as a list of pairs of primes
 * and their multiplicity for some number. The if pi divides n vi times then
 * this contains the pair (pi,vi) and if the list contains (qi, vi) then
 * qi divides n v1 times*)
type factors = (integer * integer) list

(*this is the module that allows manipulating the *)
module PMap = Map.Make(String)

(*AF: the enviroment maps ids as strings to values that that variable
 *RI: none*)
type env = value PMap.t

(* AF: a function is a closure which can be placed on the stack, the closure is
 * represented by a triple the first element is the
 * enviroment it was created in (which is stored for lexical scoping)
 * the second is the list of argument names for the function
 * the third part is the stack program that is bound to that function
 * RI: none*)
and func = env * string list * string

(*AF: these are the types of values in the laungage
 * a value in this language can come in many types the only user definable types
 * are strings, numbers(integers and floats) and matricies, the rest are used
 * internally for computation, any of these types may be placed on the stack for
 * evaluation
 * RI: the union of all the RI for the composing types
 *)
and value = S of string | N of number | M of matrix | E of exn |
             PubKey of public_key | PrivKey of private_key |
             Fact of factors | P of pair | Func of func
(*AF: a pair is a pairing of two values
 *RI: the union of all the RI for the composing types of value*)
and pair = value * value

