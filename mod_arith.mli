(*arithmatic with congruence classes*)

(*[as_big_int v] is x if v takes the form N(I(x))
  Precondition: v takes the form N(I(x))*)
val as_big_int: Types.value -> Types.integer

(*[gen_unit n] is number 0 <= r < n such that there exits r^-1
  such that r*r^-1 = 1 (mod n)
  Precondition: n >= 2*)
val gen_unit: Types.integer ->  Types.integer

(*[inv a n] is the multiplicative inverse of a mod n, if the inverse exists
  and is an exception value if n <= 0 or a has no inverse mod n*)
val inv: Types.integer -> Types.integer -> Types.value

(*[add a b n] is the unique 0 <=r < n such that (a + b - r) divides n
  if n <= 0 then the result will be an exception value*)
val add: Types.integer -> Types.integer -> Types.integer -> Types.value

(*[subtract a b n] is the unique 0 <= r < n such that (a - b - r) divides n
  if n <= 0 then the result will be an exception value*)
val subtract: Types.integer -> Types.integer -> Types.integer -> Types.value

(*[eq a b n] is non-zero if a and b are congruent mod n or the zero value if
  a b are not congruent mod n, and if n <= 0 then the result will be an
  exception value *)
val eq: Types.integer -> Types.integer -> Types.integer -> Types.value

(*[multiply a b n] is the unique 0 <= r < n such that (a*b - r) divides n
  or if n <= 0 then the result will be an exception value*)
val multiply: Types.integer -> Types.integer -> Types.integer -> Types.value

(*[divide a b n] is the unique 0 <= r < n such that (a*b' - r) divides n, where
  if [multiply b b' n] is 1, if no such b' exists or
  if n <= 0 then the result will be an exception value*)
val divide: Types.integer -> Types.integer -> Types.integer -> Types.value

(*[power a b n] is the unique 0 <= r < n such that (a^b -r) divides n if b >=0,
  instead the unique 0 <= r < n such that (a'^-b -r) divides n if b < 0, or
  if n <= 0 then the result will be an exception value*)
val power: Types.integer -> Types.integer -> Types.integer -> Types.value

(*[gcd a b] is the greatest natural number d such
  that d divides a and d divides b
  note: if a or b <= 0 then [gcd a b] will be the same as gcd applied
  to the absolute value of those two numbers*)
val gcd: Types.integer -> Types.integer -> Types.value

(*[lcm a v] is the smallest natural number n
  such that a divides n and v divides n*)
val lcm: Types.integer -> Types.integer -> Types.value

(*[gen_prime l] is a psuedorandomly generated natrual number n with a high
   probability being prime that is l bits and which given a large l
   may take a long time. If l is 1 or 0 an exception value is given*)
val gen_prime: Types.integer -> Types.value

(*[factor n] is a list of pairs of n's prime factors and their multiplicity,
  ordered from least to greatest if n is positive, and -n's prime factors
  ordered from least to greatest if n is
  negative. It is the empty list, if n=0 or n=1.*)
val factor: Types.integer -> Types.value

(*[is_prime n] is 1 (ie true) if n is a prime, 0 (ie false) if n is not a prime
  Can take an excedingly long time for large numbers*)
val is_prime: Types.integer -> Types.value

(*[is_prime_prob n] This is a faster version of is_prime, but without
   absolute certainty of correctness of result*)
val is_prime_likely: Types.integer -> Types.value

(*[totient n] is the number of units in Z mod n. In other words it is the
  result of applying the Euler totient (phi) function to n
  if n <= 0 then the result will be an exception value*)
val totient: Types.integer -> Types.value

(*[bezout x y c] is a pair (a, b) where x*a + y*b = c, or an exeption value
  if no such pair exists*)
val bezout: Types.integer -> Types.integer -> Types.integer -> Types.value

(*[crt lst1 lst2] is a pair (a,M) such that any integer n congruent to a mod M
  satisfies n = bi (mod mi) for any 0 <= bi <= j ,
  where lst1 = [b0,b1,...,bj] and lst2 = [m0,m1,...,mj].
  Precondition: all elements of lst2 are pairwise relatively prime, and greater
  than 0, and lst1 and lst2 are of the same length*)
val crt: Types.integer list -> Types.integer list -> Types.value

(*[is_square a p] is 1 if x^2 = a (mod p) for some x,
  0 if x^2 != a (mod n) for any x, and an exception value if p <= 0*)
val is_square: Types.integer -> Types.integer -> Types.value
