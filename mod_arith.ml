open Types
open Big_int
include Random

(*is_init representes the truth of the statement, the Randomizer has been
  initialized with a random seed
  RI: is_init is true iff the randomizer has been initialized*)
let is_init = ref false

(*[as_big_int i] is the big integer value of x, where i is Types.N(I(x))
  Precondtion: i is of the form Types.N(I(x))*)
let as_big_int i =
  match i with
  | N(I(x)) -> x
  | _ -> failwith "precondition violated"

(*[as_big_int_pair p] is (x,y) where p takes the form Types.P(N(I(x),N(I(y)))
  Precondition: p is of the form Types.P(N(I(x)),N(I(y)))*)
let as_big_int_pair p =
  match p with
  | P(N(I(x)),N(I(y))) -> (x,y)
  | _ -> failwith "precondition violated"

(*[truthy b] is true if b = N(I(x)) and x is not zero_big_int else false
  Precondition: b is of the form N(I(x))*)
let truthy b =
  match b with
  | N(I(x)) -> if eq_big_int x zero_big_int then false else true
  | _ -> failwith "precondition violated"

(*[add a b n] is a + b (mod n) expressed as N(I(r)) with 0 <= r < n or
  E("cannot take the remainder mod a non-positive number") is n <= 0*)
let add a b n =
  if (le_big_int n zero_big_int)
  then E("cannot take the remainder mod a non-positive number")
  else N(I(mod_big_int (add_big_int a b) n))

(*[subtract a b n] is a - b (mod n) expressed as N(I(r) with 0 <= r < n or
  E("cannot take the remainder mod a non-positive number") is n <= 0*)
let subtract a b n =
  if (le_big_int n zero_big_int)
  then E("cannot take the remainder mod a non-positive number")
  else N(I(mod_big_int (sub_big_int a b) n))

(*[multiply a b n] is a*b (mod n) expressed as N(I(r) with 0 <= r < n or
  E("cannot take the remainder mod a non-positive number") is n <= 0*)
let multiply a b n =
  if (le_big_int n zero_big_int)
  then (E "cannot take the remainder mod a non-positive number")
  else N(I(mod_big_int (mult_big_int a b) n))

(*[as_bin_list a accum] is a list of a's bits staring with accum and in reverse order
  Precondition: a >= 0*)
let rec as_bin_list a accum =
  if eq_big_int a zero_big_int then accum
  else let (q,r) = quomod_big_int a (big_int_of_int 2) in
    if eq_big_int r zero_big_int then as_bin_list q (zero_big_int::accum)
    else as_bin_list q (unit_big_int::accum)

(*[repeated_square a n exp accum] is a list squares::accum where
  squares = (a^2^0 mod n,a^2^1 mod n,..., a^2^exp mod n)
  Precondtion: n > 0 and exp >= 0*)
let rec repeated_square a n exp accum =
  match accum with
  | [] -> repeated_square a n exp ((mod_big_int a n)::accum)
  | h::t ->
    let x_sqr= as_big_int (multiply h h n) in
    if eq_big_int exp zero_big_int then accum
    else repeated_square a n (pred_big_int exp) (x_sqr::h::t)

(*[condense n pows bin accum] is N(I(r)) where 0 <= r < n and r is congruent
  to a (mod n) for some a such that pows is
  (a^2^0(mod n),a^2^1(mod n)),...,a^2^m(mod n)) and bin is a list of length m+1:
  (b0,b1,...,bm) where each b0, b1 is either 1 or 0 and a is congruent mod n
  to a^2^0*b0 + a^2^1+b1 +...+ a^2^m*bm
  Precondition: pows and bin must be the same length, n > 0*)
let rec condense n pows bin accum=
  match pows,bin with
  | [],[] -> accum
  | h1::t1,h2::t2 ->
    if (eq_big_int h2 zero_big_int)
    then condense n t1 t2 accum
    else condense n t1 t2 (as_big_int (multiply h1 accum n))
  | _,_ -> failwith "precondition violated"

(*[power a b n] is E("cannot take the remainder mod a non-positive number")
  if n <= 0, else if b < 0 let r = a ^ -b (mod n), else if b = 0 let r = 1
  else a ^ b (mod n), and give as result N(I(x)) where x = r (mod n) and
  0 <= x < n*)
let power a b n =
  let b = abs_big_int b in
  if (eq_big_int b zero_big_int) then N(I(unit_big_int))
  else if (le_big_int n zero_big_int)
  then E("cannot take the remainder mod a non-positive number")
  else let a_red = mod_big_int a n in
  if eq_big_int a_red zero_big_int then N(I(zero_big_int))
  else let bin = as_bin_list b [] in
  let expn = big_int_of_int ((List.length bin) - 1) in
  let squares = repeated_square a n expn [] in
    N(I(condense n squares bin unit_big_int))

(*[pow_factor p n zero_big_int] is e where p^e is the largest power of
  p dividing n
  Precondition: p is prime, p divides n*)
let rec pow_factor p n accum=
  let r = (mod_big_int n p) in
  if (eq_big_int r zero_big_int)
  then pow_factor p (div_big_int n p) (add_big_int accum (big_int_of_int 1))
  else accum

(*[factor_helper n d accum] is pows::accum where pows is a list of the form
  (p1,e1,...,pm,em) where the pi's are the prime factors of |n| greater than
  or equal to d, each with multiplicity ei, and such that p1 > p2 > ... > pm
  Precondtion: d,n>0*)
let rec factor_helper n d accum =
  if (eq_big_int n unit_big_int) then accum
  else if (gt_big_int (square_big_int d) n)
  then (n,big_int_of_int 1)::accum
  else let pow = pow_factor d n (zero_big_int) in
    if (eq_big_int pow zero_big_int)
    then factor_helper n (add_big_int d (big_int_of_int 1)) accum
    else let n' = div_big_int n (power_big_int_positive_big_int d pow) in
      factor_helper n' d ((d,pow)::accum)

(*[factor n] is Fact(p1,e1,...,pm,em) where the pi's are the prime factors
  of |n| in increasing order of multiplicy ei
  if |n| >= 2 or Fact([]) otherwise *)
let factor n =
  let n_pos = abs_big_int n in
  if ((eq_big_int n_pos zero_big_int) || (eq_big_int n_pos unit_big_int))
  then Fact([])
  else Fact(List.rev (factor_helper n_pos (big_int_of_int 2) []))

(*[is_prime n] is N(I(unit_big_int) if n is a prime number,
  is N(I(zero_big_int)) if n is not a prime number, may take a long
  time for large n)*)
let is_prime n =
  if (le_big_int n zero_big_int) then N(I(zero_big_int))
  else let res = factor n in
  match res with
  | Fact factors -> begin
      match factors with
      | [] -> N(I(zero_big_int))
      | (fact,freq)::t ->
        if ((eq_big_int freq unit_big_int) && (t = []))
        then N(I(big_int_of_int 1))
        else N(I(zero_big_int))
    end
  | _ -> failwith "factored incorrectly, unreachable case"

(*[eq a b n] is N(I(unit_big_int)) if a = b (mod n) and N(I(zero_big_int))
  if a != b (mod n), or E("cannot take the remainder mod a non-positive number")
  if n <= 0*)
let eq a b n =
  match subtract a b n with
  | E e -> E e
  | N v1 -> begin
      match v1 with
      | I v2 -> if (eq_big_int v2 zero_big_int) then N(I(unit_big_int))
        else N(I(zero_big_int))
      | _ -> failwith "Unreachable case"
    end
  | _ -> failwith "Unreachable case"

(*[gcd a b] is the greatest common divisor of a and b, in other words it is
  N(I(n))w where n is the largest non-negative number,
  such that n | a and n | b*)
let rec gcd a b =
  if (lt_big_int a zero_big_int) then gcd (minus_big_int a) b
  else if (lt_big_int b zero_big_int) then gcd a (minus_big_int b)
  else if (lt_big_int a b) then gcd b a
  else if (eq_big_int b zero_big_int) then N(I(a))
  else gcd b (mod_big_int a b)

(*[lcm a b] is N(I(n)) where n is the smallest non_negative number such that
  a | n and b | n*)
let lcm a b =
  match gcd a b with
  | N (I v) -> if (eq_big_int v zero_big_int) then N(I(zero_big_int))
    else N(I (div_big_int (mult_big_int a b) v))
  | _ -> failwith "unreachable case"

(*[ensure_rand_init ()] inititializes Random if !is_init is false and then
  assign !is_init to true, evalutes to value of type unit*)
let ensure_rand_init _ =
  if not(!is_init) then begin Random.self_init (); is_init := true end
  else ()

(*[gen_rand_bits_helper num_bits accum] is a list of bits::accum where
  bits is a list of length numb_bits that contains elements sampled uniformly
  at random from {zero_big_int,unit_big_int} as a side effect
  Random is initialized if it were not already
  Precondtion: n > 1*)
let rec gen_rand_bits_helper num_bits accum =
  ensure_rand_init ();
  if eq_big_int num_bits zero_big_int then accum
  else let is_one = Random.bool () in
    if is_one
    then gen_rand_bits_helper (pred_big_int num_bits) (unit_big_int::accum)
    else gen_rand_bits_helper (pred_big_int num_bits) (zero_big_int::accum)

(*[gen_rand_bits num_bits] is a list of num_bits elements which are sampled
  uniformly at random from {zero_big_int,unit_big_int} Random is initialized
  if it were not already
  Precdition: num_bits >= 0*)
let rec gen_rand_bits num_bits = gen_rand_bits_helper num_bits []

(*[big_int_of_bit_list_helper bits pow accum] is the big_int a such that
  the elements of bits, read from head to tail, form the binary representation
  of a times 2^pow
  Precondtion: pow >= 0, bits is a list with elements in {zero_big_int,unit_big_int}*)
let rec big_int_of_bit_list_helper bits pow accum =
  match bits with
  | [] -> accum
  | h::t ->
    if (eq_big_int h unit_big_int)
    then let pow_two = power_big_int_positive_big_int (big_int_of_int 2) pow in
      big_int_of_bit_list_helper t (succ_big_int pow) (add_big_int accum pow_two)
    else big_int_of_bit_list_helper t (succ_big_int pow) accum

(*[big_int_of_bit_list] is the big_int a such that, when read from head to tail,
  the elements of bits form the binary representation for a*)
let big_int_of_bit_list bits =
  big_int_of_bit_list_helper bits zero_big_int zero_big_int

(*[gen_rand_big_int n] is a big_int sampled uniformly at random from the
  set of big_int's representable as an unsigned binary integer with n bits
  Precondtion: n > 0*)
let gen_rand_big_int n = big_int_of_bit_list (gen_rand_bits n)

(*[gen_num_bits_helper n accum] is the minimal number of bits that can be
  used to represent n as an unsigned binary integer times 2^accum
  Precondtion: n >= 0*)
let rec get_num_bits_helper n accum =
  if eq_big_int zero_big_int n then accum
  else get_num_bits_helper (div_big_int n (big_int_of_int 2)) (succ_big_int accum)

(*[gen_num_bits n] is the minimal number of bits that can be
  used to represent n as an unsigned binary integer
  Precondtion: n >= 0*)
let get_num_bits n = get_num_bits_helper n zero_big_int


(*[fermats_little a n] is true if a^(n-1) = a (mod n), false otherwise
  Precondtion: n > 0*)
let fermats_little a n =
  let result = power a (pred_big_int n) n in
  if (eq_big_int (as_big_int result) unit_big_int) then true
  else false

(*[is_prime_k_tests_helper n k num_bits] is true iff [fermats_little a n] is
  not false for any of k randomly sampled numbers, a, representable as unsinged
  integers with num_bits bits
  Precondtion: num_bits >= 1, n > 0, k >= 0*)
let rec is_prime_k_tests_helper n k num_bits =
  if (eq_big_int k zero_big_int) then true
  else let a = gen_rand_big_int num_bits in
    if (fermats_little a n)
    then is_prime_k_tests_helper n (pred_big_int k) num_bits
    else false

(*[is_prime_k_test n k] is true iff [fermats_little a n] is
  not false for any of k randomly sampled numbers, a, whose are representable
  as unsigned binary integers with at most n-1 bits
  Precondtion: k >= 0*)
let rec is_prime_k_tests n k =
  (*hardcoded for now*)
  if lt_big_int n (big_int_of_int 1000000) then
    truthy (is_prime (n))
  else let bits = get_num_bits n in
    is_prime_k_tests_helper n k bits

(*[is_prime_likely n] is N(I(unit_big_int) if n is exceedingly* likely to be
  prime, or N(I(zero_big_int) if n is certainly composite
  *Note exceedingly likely means with probability roughly 1/2^100 of being
  neither a Carmichael number (psuedoprime) or prime. Where the density of
  psueodprimes decreases rapidly as n increases
  *)
let is_prime_likely n =
  let prob_prime = is_prime_k_tests n (big_int_of_int 100) in
  if prob_prime then N(I(unit_big_int))
  else N(I(zero_big_int))

(*[gen_prime_helper n] is p which is exceedingly* likely to be a prime which is
  representable with at most n bits
  Precondtion: n >= 2
  *exceedingly likely to be prime defined above*)
let rec gen_prime_helper n =
  let p = gen_rand_big_int n in
  if truthy (is_prime_likely p) then p
  else gen_prime_helper n

(*[gen_prime l] is N(I(p)) where p is represnetable with at most l bits
  and is exceedingly* likely to be a prime if n > 1,
  or if n <= 1, is E("no primes this small")
  *exceedinly likely to be prime defined above*)
let gen_prime l =
  if (le_big_int l unit_big_int) then E("no primes this small")
  else N(I(gen_prime_helper l))

(*[gen_unit n bits] is a number 0 <= a < n such that there exists a number
  0 <= b < n and if r = a*b (mod n), then r = 1 (mod n) and a can be represented
  as an unsigned interger with at most bits number of bits
  Precondtion: n > 1 (in other words Z/nZ has a unit)**)
let rec gen_unit_helper n bits =
  let u = gen_rand_big_int bits in
  if eq_big_int (as_big_int (gcd u n)) unit_big_int then u
  else gen_unit_helper n bits

(*[gen_unit n] is a number 0 <= a < n such that there exists a number
  0 <= b < n and if r = a*b (mod n), then r = 1 (mod n)
  Precondtion: n > 1 (in other words Z/nZ has a unit)*)
let gen_unit n =
  let num_bits = get_num_bits n in
  gen_unit_helper n (pred_big_int num_bits)

(*[totient_helper factors accum] is the euler totient function applied to
  the number n times accum, where n is the number represted as factors,
  where factors = (p1,e1,...,pn,en) are all distinct prime
  factors of n and mulptilicites of those factors, respectively
  Precondtion: factors is a list of tuples of non_negative big_ints*)
let rec totient_helper factors accum =
  match factors with
  | [] -> accum
  | (factor,pow)::t ->
    let term1 = (power_big_int_positive_big_int factor pow) in
    let term2 = div_big_int term1 factor in
    let accum' = mult_big_int (sub_big_int term1 term2) accum in
    totient_helper t accum'
(*[totient n] is E("totient undefined for non_positive values")
  if n is less than or equal to zero_big_int,
  otherwise it is N(I(u)) where u is the result
  of apply the euler totient function to n*)
let totient n =
  if (le_big_int n zero_big_int) then E("totient undefined for non_positive values")
  else let res = factor n in
  match res with
  | Fact factors -> N(I(totient_helper factors unit_big_int))
  | _ -> failwith "n must be factorable"

(*[gen_bezout_ceofs a b coefs] is a list of (v,(x',a'),(b') such that
  v = x'*a' + b', for each of the values a',b' that occur when recursively
  applying the euclidian gcd function to a and b*)
let rec gen_bezout_coefs a b coefs =
  if (eq_big_int b zero_big_int) then (a,(zero_big_int,zero_big_int),a)::coefs
  else let (q,r) = quomod_big_int a b in
    let coefs' = (a,(q,b),r)::coefs in
    gen_bezout_coefs b r coefs'

(*[merg_coefs ((x,b),(y,r),r') ((a),(q,b),r)] is the result of using the
  back-substitution on the pair of equations x*b = y*r + r' and a = q*b + r
  to generate a new, valid solution of the form ((x',a),(y',b),r')*)
let merge_coefs ((x,b),(y,r),r') ((a),(q,b),r) =
  let x' = minus_big_int y in
  let y' = minus_big_int (add_big_int x (mult_big_int y q)) in
  ((x',a),(y',b),r')

(*[construct_min_bezout_sol eqn coef] is the solution with minimal coeficients
  to the equation a*x+b*y = gcd(a,b) where coefs are the coeficients generated
  from applying the recursive euclidian gcd algorithm to a,b.
  Precondtion: coefs is empty, or has valid coeficients*)
let rec construct_min_bezout_sol eqn coefs =
  match coefs with
  | [] -> eqn
  | h::t -> construct_min_bezout_sol (merge_coefs eqn h) t

(*[get_x_y_gcd coefs] is (x,y,gcd) where coefs are the coeficients
  generated from  applying the recursive euclidian gcd algorithm to a,b and
  a*x + b*y = gcd(a,b) = gcd
  Precondtion:coefs is not empty and has valid coeficients*)
let get_x_y_gcd coefs =
  match coefs with
  | [] -> failwith "error cannot have no coefficients"
  | ((a),(q,b),r)::t ->
    let ((x,_),(neg_y,_),res) =
      construct_min_bezout_sol ((unit_big_int,a),(q,b),r) t in
    (x,minus_big_int neg_y, res)

(*[bezout a b c] is a pair (x, y) where x*a + y*b = c, or an exception value
  if no such pair exists*)
let bezout a b c =
  let coefs = gen_bezout_coefs a b [] in
  let (x,y,res) = get_x_y_gcd coefs in
  if eq_big_int zero_big_int (mod_big_int c res)
  then let m = div_big_int c res in
    P(N(I(mult_big_int m x)),N(I(mult_big_int m y)))
  else E("gcd(a,b) does not divide c, so no solution exists")

(*[join_congruence_pair bi mi bj mj] is P(a,m) where if x = bi (mod mi)
  and x = bj (mod mj), then it is necissary and sufficient that
  x = a (mod m)
  Precondtion: mi mj are mutually prime, and mi,mj > 0*)
let join_congruence_pair bi mi bj mj =
  if not(eq_big_int (as_big_int (gcd mi mj)) unit_big_int)
  then E("not relatively prime")
  else let m' = mult_big_int mi mj in
    let coefs = as_big_int_pair (bezout mi mj unit_big_int) in
    let fst_term = as_big_int (multiply (mult_big_int (fst coefs) mi) bj m') in
    let snd_term = as_big_int (multiply (mult_big_int (snd coefs) mj) bi m') in
    let res = (add fst_term snd_term m') in
    P(res,N(I(m')))

(*[crt_helper lst1 lst2 accum] is a pair (a,M) such that any
  integer n congruent to a mod M satisfies n = bi (mod mi) for any 0 <= bi <= j
  and n = q (mod m') if accum = (q,m')
  where lst1 = [b0,b1,...,bj] and lst2 = [m0,m1,...,mj].
  Precondtion: all elements of lst2 (with the additional element of (snd accum )
  if it is exsits) are pairwise relatively prime, and greater
  than 0, and lst1 and lst2 are of the same length*)
let rec crt_helper lst1 lst2 accum =
  match lst1,lst2 with
  | [],[] -> accum
  | bi::t1,mi::t2 ->
    let (b,m) = as_big_int_pair accum in
    let accum' = join_congruence_pair bi mi b m in
    crt_helper t1 t2 accum'
  | _,_ -> E("lists are not the same length")

(*[crt lst1 lst2] is a pair (a,M) such that any integer n congruent to a mod M
  satisfies n = bi (mod mi) for any 0 <= bi <= j ,
  where lst1 = [b0,b1,...,bj] and lst2 = [m0,m1,...,mj].
  Precondition: all elements of lst2 are pairwise relatively prime, and greater
  than 0, and lst1 and lst2 are of the same length and non-empty*)
let crt lst1 lst2 =
  match lst1,lst2 with
  | [],_ -> E("must supply at least one congruence equation")
  | _,[] -> E("must supply at least one congruence equation")
  | b0::t1,m0::t2 -> crt_helper t1 t2 (P(N(I(b0)),N(I(m0))))

(*[is_square a p] is N(I(1)) if x^2 = a (mod n) for some x,
  N(I(0)) if x^2 != a (mod n) for any x, or
  E("cannot take the remainder mod a non-positive number") if n <= 0*)
let is_square a p =
  let two = big_int_of_int 2 in
  if (le_big_int p zero_big_int)
  then E("cannot take the remainder mod a non-positive number")
  else if (le_big_int p two) then N(I(unit_big_int))
  else
    let a_red = mod_big_int a p in
    let p_minus_one = pred_big_int p in
    let p_minus_one_div_two = div_big_int p_minus_one two in
    let legendre = as_big_int (power a_red p_minus_one_div_two p) in
    if (eq_big_int legendre (p_minus_one)) then N(I(zero_big_int))
    else N(I(unit_big_int))

(*[inv a n] is E("cannot take the remainder mod a non-positive number")
  if n <= 0, if n > 0, and there exists some b such that a*b = 1 (mod n), then
  N(I(b)), otherwise, E("has no inerse mod this number") *)
let inv a n =
  if (le_big_int n zero_big_int)
  then E("cannot take the remainder mod a non-positive number")
  else let result = bezout a n (big_int_of_int 1) in
      match result with
        | E _ -> E("has no inverse mod this number")
        | P (N(I(x)),_) -> N(I(mod_big_int x n))
        | _ -> failwith "Unimplemented"

(*[divide a b n] if E("cannot take the remainder mod a non-positive number") if
  n <= 0, if n > 0 then if b has multiplicative inverse mod n, b^-1, and
  r = a*b^-1 (mod n) for 0 <= r < n, it is N(I(r)), otherwise
  E("second arguement is not relatively prime to divisor")*)
let divide a b n =
  if (le_big_int n zero_big_int)
  then E("cannot take the remainder mod a non-positive number")
  else let result = inv b n in
  match result with
    | E _ -> E("second arguement is not relatively prime to divisor")
    | N(I(b_inv)) -> multiply a b_inv n
    | _ -> failwith "Unimplemented"
