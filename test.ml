open OUnit
open Types


let simple_lang_tests = [
  (*tests basic integer*)
  ("simple_int", "4", "4");
  (*tests a negative integer*)
  ("negative_int", "-78", "-78");
  (*tests an int that would normally overflow*)
  ("overflowing_int", "1000000000000000000000000000000000000",
    "1000000000000000000000000000000000000" );
  (*tests a simple floating point value  *)
  ("simple_float", "5.0", "5.");
  (*tests a larger float which is negative*)
  ("negative_float", "-3479.2498", "-3479.2498");
  (*tests simple 4 by 4 matrix*)
  ("simple_matrix", "[[2., 3.], [4., 5.]]", "[\n[ 2. 3. ]\n[ 4. 5. ]\n]");
  (*tests a simple 4 by 4 matrix with negative integers*)
  ("simple_matrix_with_neg", "[[2., -3.], [-4., 5.]]",
    "[\n[ 2. -3. ]\n[ -4. 5. ]\n]");
  (*tests a simple string*)
  ("simple_string", {|"message"|}, "message");
  (*tests a simple if, if the value if the first one is a false (0) value*)
  ("simple_if_false", "0 2 3 ?", "3");
  (*tests a simple if, if the value if the first one is a true (non 0) value*)
  ("simple_if_true", "1 2 3 ?", "2");

]

let lang_tests = [
  (*a majority of these tests were done interactivly, but here are some simple
   * tests to avoid reversion*)
  ("var defn", ["{x :-> 5}"; "x"], "5");
  ("simpl fun defn", ["{add2 : x -> x 2 +}"; "4 add2"], "6");
  ("simpl 2 param fun defn", ["{add : x y -> x y +} "; "4. 7. add"], "11.");
]

let simpl_arith_tests = [
  (*-------- + --------*)
  (*tests a simple addition of integers*)
  ("simple_integer_add", "2 3 +", "5");
  ("simple_integer_commutative_add", "3 2 +", "5");
  ("simple_integer_zero_add", "0 0 +", "0");
  ("simple_integer_zero_and_other1_add", "0 1 +", "1");
  ("simple_integer_zero_and_other2_add", "1 0 +", "1");
  ("simple_integer_zero_and_other3_add", "0 -1 +", "-1");
  ("simple_integer_zero_and_other4_add", "0 -5 +", "-5");
  ("simple_integer_zero_and_other5_add", "0 5 +", "5");

  (*tests addition of simple floating point numbers*)
  ("simple_float_add", "2. 3. +", "5.");
  ("simple_float_communtative_add", "3. 2. +", "5.");
  ("simple_float_zero_add", "0. 0. +", "0.");
  ("simple_float_zero_and_other1_add", "0. 1. +", "1.");
  ("simple_float_zero_and_other2_add", "1. 0. +", "1.");
  ("simple_float_zero_and_other3_add", "0. -1. +", "-1.");
  ("simple_float_zero_and_other4_add", "0. -5. +", "-5.");
  ("simple_float_zero_and_other5_add", "0. 5. +", "5.");

  (*-------- - --------*)
  (*tests simple subtraction of integers which results in a negative integer*)
  ("simple_integer_subt", "2 3 -", "-1");
  (*tests simple subtraction of floating point numbers*)
  ("simple_float_subt", "10. 3. -", "7.");
  ("simple_float_zero_sub", "0. 0. +", "0.");
  ("simple_float_zero_and_other1_sub", "0. 1. -", "-1.");
  ("simple_float_zero_and_other2_sub", "1. 0. -", "1.");
  ("simple_float_zero_and_other3_sub", "0. -1. -", "1.");
  ("simple_float_zero_and_other4_sub", "0. -5. -", "5.");
  ("simple_float_zero_and_other5_sub", "0. 5. -", "-5.");

  (*-------- * --------*)
  (*tests simple multiplication of integer values*)
  ("simple_integer_mult", "9 5 *", "45");
  ("simple_integer_zero_and_other1_mult", "0 1 *", "0");
  ("simple_integer_zero_and_other2_mult", "1 0 *", "0");
  ("simple_integer_zero_and_other3_mult", "0 -1 *", "0");
  ("simple_integer_zero_and_other4_mult", "0 -5 *", "0");
  ("simple_integer_zero_and_other5_mult", "0 5 *", "0");
  (*tests multiplication of floating point numbers*)
  ("simple_float_mult", "2.5 7. *", "17.5");
  ("simple_float_zero_and_other1_mult", "0. 1. *", "0.");
  ("simple_float_zero_and_other2_mult", "1. 0. *", "0.");
  ("simple_float_zero_and_other3_mult", "0. -1. *", "0.");
  ("simple_float_zero_and_other4_mult", "0. -5. *", "0.");
  ("simple_float_zero_and_other5_mult", "0. 5. *", "0.");

  (*-------- / --------*)
  (*tests simple integer division*)
  ("simple_integer_div", "8 2 /", "4");
  ("simple_integer_zero_and_other1_div", "0 1 /", "0");
  ("simple_integer_zero_and_other2_div", "1 0 /", "division by 0");
  ("simple_integer_zero_and_other3_div", "0 -1 /", "0");
  ("simple_integer_zero_and_other4_div", "0 -5 /", "0");
  ("simple_integer_zero_and_other5_div", "0 5 /", "0");
  (*tests simple floating point division*)
  ("simple_float_div", "27. 5. /", "5.4");
  ("simple_float_zero_and_other1_div", "0. 1. /", "0.");
  ("simple_float_zero_and_other2_div", "1. 0. /", "inf");
  ("simple_float_zero_and_other3_div", "0. -1. /", "0.");
  ("simple_float_zero_and_other4_div", "0. -5. /", "0.");
  ("simple_float_zero_and_other5_div", "0. 5. /", "0.");

  (*-------- ^ --------*)
  (*tests powers of the small floating point numbers*)
  ("simple_float_pow", "10. 3. ^", "1000.");
  ("simple_float_pow_0._0.", "0. 0. ^", "undefined");
  ("simple_float_pow_sqrt", "25. 0.5 ^", "5.");
  ("simple_float_pow_inverse", "10. -1. ^", "0.1");
  (*tests powers of the large floating point numbers*)
  ("big_simple_float_pow", "1000. 3. ^", "1000000000.");
  ("big_simple_float_pow_sqrt", "25000. 0.5 ^", "158.113883008");
  ("big_simple_float_pow_inverse", "10000000. -1. ^", "1e-07");

  (*tests powers of the small integer numbers*)
  ("simple_integer_pow", "10 3 ^", "1000");
  ("simple_integer_pow_0._0.", "0 0 ^", "undefined");
  ("simple_integer_pow_inverse", "10 -1 ^", "I do not understand");
  (*tests powers of the large integer numbers*)
  ("big_simple_float_pow", "1000 3 ^", "1000000000");
  ("big_simple_float_pow_sqrt", "25000 0.5 ^", "Incorrect Types");
  ("big_simple_float_pow_inverse", "10000000 -1 ^", "I do not understand");


  (*-------- % --------*)
  (*tests modulus for integers*)
  ("simple_integer_mod", "100 3 %", "1");
  ("simple_integer_mod1", "100 0 %", "I do not understand");
  ("simple_integer_mod2", "0 0 %", "I do not understand");
  ("simple_integer_mod3", "0 100 %", "0");
  ("simple_integer_mod4", "-5 -4 %", "-1");
  ("simple_integer_mod4", "-1 -1 %", "0");


  (*-------- = --------*)
  (*tests the equality of two numbers*)
  ("simple_integer_=", "18 17 =", "0");
  ("simple_integer_reverse_=", "17 18 =", "0");
  ("simple_integer_=_1", "0 0 =", "1");
  ("simple_integer_=_2", "0 1 =", "0");
  ("simple_integer_=_3", "4 0 =", "0");
  (*tests the equality of floating point numbers*)
  ("simple_float_=", "15.2 15.3 =", "0");
  ("simple_integer_reverse_=", "15.3 15.2 =", "0");
  ("simple_integer_=_1.", "0. 0. =", "1");
  ("simple_integer_=_2", "0. 1. =", "0");
  ("simple_integer_=_3", "4. 0. =", "0");
]

let mod_arith_tests = [
  (*black box*)
  (*test addition of small numbers whose sum is not divisable by the modulo*)
  ("simple_mod_add", "7 6 2 +~", "1");
  (*tests addition with some larger numbers*)
  ("larger_mod_add", "21 34 7 +~", "6");
  (*tests the addition of two numbers which is not divisible by the modulo
   * but their sum is*)
  ("non_div_mod_add", "5 6 11 +~", "0");
  (*test the addition two numbers mod 0*)
  ("add_mod_zero","4 5 0 +~","cannot take the remainder mod a non-positive number");
  (**test the addition two numbers mod a negative number*)
  ("add_mod_neg","4 0 -63 +~","cannot take the remainder mod a non-positive number");
  (*tests the simple modular subtraction of 2 numbers which is not divisable by
   * the modulo*)
  ("simple_mod_sub", "9 4 2 -~", "1");
  (*tests the subtraction of the same number which would be 0 regardless of mod*)
  ("0_mod_sub", "7 7 20 -~", "0");
  (*tests the subtraction of two numbers which are non zero, but is divisable
   * by the modulo*)
  ("no_div_mod_sub", "37 20 17 -~", "0");
  (*tests the subtraction of two larger numbers*)
  ("larger_mod_sub", "483275 34261 3 -~", "1");
  (*tests the subtraction of two numbers mod 0*)
  ("sub_mod_zero","4 5 0 -~","cannot take the remainder mod a non-positive number");
  (*tests the subtraction of two numbers mod a negative number*)
  ("sub_mod_zero","4 5 -1 -~","cannot take the remainder mod a non-positive number");
  (*tests simple multiplication, which us not divisable by the modulo*)
  ("simple_mod_mult", "6 7 5 *~", "2");
  (*tests the multiplication of two numbers which is divisable by the modulo*)
  ("div_mod_mult", "9 4 6 *~", "0");
  (*tests multiplication by 0*)
  ("zero_mod_mult", "0 8 7 *~", "0");
  (*tests multiplication mod 0*)
  ("mult_mod_zero", "10 12 0 *~", "cannot take the remainder mod a non-positive number");
  (*tests multiplication mod a negative number*)
  ("mult_mod_zero", "10 0 -3 *~", "cannot take the remainder mod a non-positive number");
  (*tests division of two numbers for which the denom divides the numerator*)
  ("rel_prime&divis_mod_div", "49 7 5 /~", "2");
  (*tests the division of numbers which the denom does not divide the numerator
   * but the divisor is rel prime to the modulo*)
  ("rel_prime_mod_div", "9 5 7 /~", "6");
  (*tests the divions of numbers in which the denom is not rel prime to the modulous*)
  ("non_rel_prime_div", "10 4 2 /~", "second arguement is not relatively prime to divisor");
  (*tests the division of numbers mod 0*)
  ("div_mod_zero", "10 4 0 /~", "cannot take the remainder mod a non-positive number");
  (*test the divions of numbers mod a negative number*)
  ("div_mod_neg", "9 2 -1 /~", "cannot take the remainder mod a non-positive number");
  (*tests simple modular powers *)
  ("simpl_mod_pow", "3 4 11 ^~", "4");
  (*tests powers with a large power*)
  ("large_mod_pow", "2 243567633493504 5 ^~", "1");
  (*tests 0 to a power*)
  ("zero_mod_pow","0 829375 12 ^~", "0");
  (*tests 0 to a pow mod 0*)
  ("zero_pow_mod_zero","0 829375 0 ^~", "cannot take the remainder mod a non-positive number");
  (*tests a number to the 0 mod a number*)
  ("to_the_zero","12 0 7 ^~","1");
  (*tests powers mod 0*)
  ("pow_mod_zero", "3285 293 0 ^~", "cannot take the remainder mod a non-positive number");
  (*tests powers mod negative number*)
  ("pow_mod_neg", "0 23 -1 ^~", "cannot take the remainder mod a non-positive number");
  (*tests powers to a negative number *)
  ("pow_to_neg","5 -7 4 ^~","1");
  (*tests simple equality of two small numbers*)
  ("simple_mod_eq", "17 2 15 =~", "1");
  (*tests the modular equality with large numbers*)
  ("large_mod_eq", "726476239857380 89771 358893 =~", "1");
  (*tests modular inequality of two numbesr*)
  ("mod_ineq","892735 8927350 97=~","0");
  (*tests equality mod 0*)
  ("mod_eq_zero","892735 8927350 0 =~","cannot take the remainder mod a non-positive number");
  (*tests equality mod -1*)
  ("mod_eq_zero","-12 0 -1 =~","cannot take the remainder mod a non-positive number");
  (*tests the gcd of two small numbers which have a common division above 1*)
  ("simple_gcd", "68 51 gcd", "17");
  (*tests the gcd of two large rel prime numbers*)
  ("large_1_gcd", "100000037 1000000345537 gcd", "1");
  (*tests the gcd of two large numbers which are not rel prime*)
  ("large_1_gcd", "8753735081401 9696994354185197 gcd", "411301747");
  (*test the gcd with one number negative*)
  ("simple_gcd_neg", "-68 51 gcd", "17");
  (*test the gcd with the second number negative*)
  ("other_gcd_neg", "68 -51 gcd", "17");
  (*test the gcd with one number 0 number negative*)
  ("gcd_zero", "68 0 gcd", "68");
  (*test the gcd with one number diving the other*)
  ("gcd_div","999999999 9 ","9");
  (*tests a small lcm*)
  ("small_lcm", "56 62 lcm", "1736");
  (*tests the lcm of two large numbers*)
  ("large_lcm", "1624956750 14873852 lcm", "326613056836500");
  (*tests the lcm of two large negative integers*)
  ("large_lcm_neg", "-1624956750 -14873852 lcm", "326613056836500");
  (*tests the lcm with one number 0*)
  ("lcm_0", "0 12 lcm", "0");
  (*tests lcm with both numbers 0*)
  ("lcm_0_0","0 0 lcm", "0");
  (*tests factoring a small number*)
  ("simple_factor", "876 factor", "(2,2) (3,1) (73,1) ");
  (*tests factoring of a large number*)
  ("largish_factor", "387153510 factor", "(2,1) (3,1) (5,1) (43,1) (300119,1) ");
  (*tests factor for a large composite number with small divisors*)
  ("large_factor_small_div","3541171240000000000 factor","(2,12) (5,10) (97,4) ");
  (*tests factor for a prime*)
  ("prime_factor","617 factor", "(617,1) ");
  (*tests factor for 0*)
  ("zero_factor","0 factor", "");
  (*tests factor for 1*)
  ("one_factor","1 factor", "");
  (*tests factor of negative number*)
  ("neg_factor", "-24 factor","(2,3) (3,1) ");
  (*test factor for -1*)
  ("neg_unit_factor", "-1 factor","");
  (*tests that a negative nubmer is not prime*)
  ("neg_not_prime","-7 is_prime","0");
  (*tests that a small composite is not prime*)
  ("small_composite_is_prime", "48 is_prime", "0");
  (*tests that a number is composite*)
  ("composite_is_prime", "387153510 is_prime", "0");
  (*tests small prime is prime*)
  ("is_prime_small","7 is_prime","1");
  (*tests large prime is prime*)
  ("is_prime_larger", "1008667313 is_prime", "1");
  (*test an is prime prob for a small number that should clearly be not prime*)
  ("small_composite_is_prime_likely", "56 is_prime_prob", "0");
  (*tests that a larger clearly composite number is not prime*)
  ("larger_is_prime_prob", "234976 is_prime_prob", "0");
  (*tests that a very large compositve nubmer is not prime*)
  ("very_large_non_prime", "856361215938558998591710718116059059061475058229 is_prime_prob", "0");
  (*tests that a very large prime is probably prime*)
  ("very_large_prime",
  "1582375376486522645799264544143004449330845988412246964181752084476603692779420515602503876711832499362684765390499372107885862811732185802600456088127 is_prime_prob",
  "1");
  (*tests invalid bits for gen primes*)
  ("0_bit_prime","0 gen_prime","no primes this small");
  (*tests invalid bits for gen primes*)
  ("1_bit_prime","1 gen_prime","no primes this small");
  (*tests generating a small prime*)
  ("small_prime","3 gen_prime is_prime","1");
  (*test generating a somewhat large prime*)
  ("large_prime","200 gen_prime is_prime_prob", "1");
  (*tests finding the totient of a prime*)
  ("prime_totient", "35738783 totient", "35738782");
  (*finds a totient of a composite*)
  ("composite_totient", "532501478 totient", "266250738");
  (*tests totient of zero*)
  ("zero_totient", "0 totient", "totient undefined for non_positive values");
  (*tests totient of 1*)
  ("one_totient", "1 totient", "1");
  (*tests for negative totient*)
  ("neg_totient","-12 totient","totient undefined for non_positive values");
  (*tests bezouts for simple relatively prime numbers*)
  ("bezout_simpl", "3 4 5 bezout", "(-5,5)");
  (*tests bezouts for non-relatively prime numbers that doesn't have solution*)
  ("bezout_simpl", "6 4 5 bezout", "gcd(a,b) does not divide c, so no solution exists");
  (*tests bezouts for negative first number, with no solution*)
  ("bezout_neg_fst", "-6 4 5 bezout", "gcd(a,b) does not divide c, so no solution exists");
  (*tests bezouts for negative second number, with no solution*)
  ("bezout_neg_snd", "6 -9 5 bezout", "gcd(a,b) does not divide c, so no solution exists");
  (*tests bezouts for negative first number*)
  ("bezout_neg_fst_soln", "-35 7 14 bezout","(0,2)");
  (*tests bezouts for negative second number*)
  ("bezout_neg_snd_soln", "12 -6 36 bezout","(0,-6)");
  (*tests bezouts first number 0*)
  ("bezout_fst_zero", "0 2 12 bezout","(0,6)");
  (*tests bezouts second number 0*)
  ("bezout_fst_zero", "1 0 17 bezout","(17,0)");
  (*tests bezouts for both negative*)
  ("bezout_both_neg","-12 -17 97 bezout","(679,-485)");
  (*tests bezout for very large mutually prime numbers*)
  ("bezout_large","429765647491 1029159179 827385723 bezout", "(73237333028758542,-30583111429080899181)");
  (*tests if a square is a square mod a number*)
  ("trivial_square","4 3 square","1");
  (*tests if a non-square is a square mod a number*)
  ("no_square","7 4 square","0");
  (*tests if negative number is square mod a number*)
  ("neg_square","-3 4 square","1");
  (*tests if number is square mod 0*)
  ("square_0","-3 0 square","cannot take the remainder mod a non-positive number");
  (*tests if number is square mod a negative number*)
  ("square_neg","3 -15 square","cannot take the remainder mod a non-positive number");
  (*tests if 2 is a square mod a number congruent to 1 mod 8*)
  ("square_2_true","2 9 square","1");
  (*tests if 2 is a square mod a number congruent to 8 mod 8*)
  ("square_2_true2","2 7 square","1");
  (*tests if 2 is a square mod a number congruent to 3 mod 8*)
  ("square_2_false","2 11 square","0");
  (*tests if 2 is a square mod a number congruent to 5 mod 8*)
  ("square_2_false2","2 13 square","0");
  (*tests is_square for a large pair of numbers*)
  ("square_large","428293582935 209409118403 square","1");
  (*tests solve in simple case*)
  ("crt_simpl","2 1 3 1 2 solve","(1,6)");
  (*tests solve in a less simple case*)
  ("crt_average","2 1 3 1 5 1 7 1 4 solve","(1,210)");
  (*tests solution for complex case*)
  ("crt_complicated","617 397 571 229 911 829 3 solve","(103607037,320951677)")
]

let comb_arith_tests = [
  (*-------- ! --------*)
  ("negative_fact", "-3 !", "Negative values are not allowed");
  (*tests that zero factorial is 1*)
  ("zero_fact", "0 !", "1");
  (*tests that a 1 factorial is 1*)
  ("one_fact", "1 !", "1");
  (*tests a small factorial*)
  ("small_fact", "6 !", "720");
  (*tests a larger factorial*)
  ("large_fact", "30 !", "265252859812191058636308480000000");
  ("inception_fact", "3 ! !", "720");

  (*-------- choose --------*)
  ("first_negative_choose", "-13 5 choose", "Negative values are not allowed");
  ("second_negative_choose", "13 -5 choose", "Negative values are not allowed");
  ("both_negative_choose", "-13 -5 choose", "Negative values are not allowed");
  ("0_0_choose", "0 0 choose", "1");
  ("1_1_choose", "1 1 choose", "1");
  ("1_5_choose", "1 5 choose", "Invalid inputs");
  ("5_1_choose", "5 1 choose", "5");
  (*computes a small combination*)
  ("small_choose", "13 5 choose", "1287");
  (*computes a large choose*)
  ("large_choose", "217 43 choose", "5601414076770489401221861478881318576914682800");

  (*-------- perm --------*)
  ("first_negative_perm", "-13 5 perm", "Negative values are not allowed");
  ("second_negative_perm", "13 -5 perm", "Negative values are not allowed");
  ("both_negative_perm", "-13 -5 perm", "Negative values are not allowed");
  ("0_0_perm", "0 0 perm", "1");
  ("1_1_perm", "1 1 perm", "1");
  ("1_5_perm", "1 5 perm", "Invalid inputs");
  ("5_1_perm", "5 1 perm", "5");
  (*computes a small permutation*)
  ("small_perm", "10 4 perm", "5040");
  (*computes a large perm*)
  ("large_perm", "49 25 perm", "980390734080409707851586040233984000000");

  (*-------- part --------*)
  ("first_negative_part", "-18 6 part", "Negative values are not allowed");
  ("second_negative_part", "18 -4 part", "Negative values are not allowed");
  ("both_negative_part", "-8 -4 part", "Negative values are not allowed");
  ("0_0_part", "0 1 part", "1");
  ("1_1_part", "0 2 part", "1");
  ("1_5_part", "-4 6 part", "Invalid inputs");
  ("5_1_part", "4 2 part", "5");
  (*computes a small partutation*)
  ("small_part", "10 4 part", "286");
  (*computes a large part*)
  ("large_part", "49 25 part", "11844267374132633700");
]

let xempty = "[]"
let a11 = "[[1.]]"
let a110 = "[[0.]]"
let b11 = "[[-7.]]"
let a21 = "[[2.], [4.]]"
let a12 = "[[2., -3.]]"
let a31 = "[[2.], [4.], [8.]]"
let b12 = "[[7., 34.]]"
let i22 = "[[1., 0.], [0., 1.]]"
let a22 = "[[2., -3.], [-4., 5.]]"
let a23 = "[[6., 2., 3.], [-2., -5., 0.]]"
let a231 = "[[1., 1., 1.], [1., 1., 1.]]"
let a220 = "[[0., 0.], [0., 0.]]"
let a210 = "[[0.], [0.]]"
let a310 = "[[0.], [0.], [0.]]"
let a221 = "[[1., 1.], [1., 1.]]"
let b22 = "[[7., 34.], [56., -19.]]"
let c22 = "[[4., 3.], [3., 2.]]"
let i33 = "[[1., 0., 0.], [0., 1., 0.], [0., 0., 1.]]"
let a13 = "[[1., 2., 3.]]"
let b13 = "[[4., 5., 6.]]"
let a33 = "[[6., 3., 5.], [6., 2., 9.], [0., -5., 1.]]"
let a330 = "[[0., 0., 0.], [0., 0., 0.], [0., 0., 0.]]"
let b33 = "[[5., 1., -7.], [0., -2., 6.], [2., 2., 8.]]"
let c33 = "[[1., 2., 3.], [0., 1., 4.], [5., 6., 0.]]"
let linear_arith_tests = [
  (*-------- + --------*)
  (*simple adding of two matricies*)
  ("simple_add_1x1", a11 ^ " " ^ b11 ^ " +",
   "[\n[ -6. ]\n]");
  ("simple_add_1x2", a12 ^ " " ^ b12 ^ " +",
   "[\n[ 9. 31. ]\n]");
  ("simple_add_2x2", a22 ^ " " ^ b22 ^ " +",
   "[\n[ 9. 31. ]\n[ 52. -14. ]\n]");
  ("simple_add_3x3", a33 ^ " " ^ b33 ^ " +",
   "[\n[ 11. 4. -2. ]\n[ 6. 0. 15. ]\n[ 2. -3. 9. ]\n]");

  (*-------- - --------*)
  (*simple subtraction of two matricies*)
  ("simple_sub_matrix", "[[5., -3.], [-10., 5.], [5.6, 7.1]] [[7., 12.], [23., -19.], [13., 5.]] -",
   "[\n[ -2. -15. ]\n[ -33. 24. ]\n[ -7.4 2.1 ]\n]");

  (*-------- scale --------*)
  (*scaling an int matrix*)
  ("simpl_float_scale", "2. [[2., 3.], [5., 7.]] scale", "[\n[ 4. 6. ]\n[ 10. 14. ]\n]");
  (*scaling an int matrix*)
  ("simpl_int_scale", "2 [[2, 3], [5, 7]] scale", "[\n[ 4 6 ]\n[ 10 14 ]\n]");
  ("simple_scale_1x10", "0." ^ " " ^ a110 ^ " scale",
   "[\n[ 0. ]\n]");
   ("simple_scale_1x1_0_scalebynonzero", "5." ^ " " ^ a110 ^ " scale",
   "[\n[ 0. ]\n]");
  ("simple_scale_2x31", "3." ^ " " ^ a231 ^ " scale",
   "[\n[ 3. 3. 3. ]\n[ 3. 3. 3. ]\n]");

  (*-------- . --------*)
  ("simple_dot_1x1", a11 ^ " " ^ b11 ^ " .",
   "-7.");
  ("simple_dot_1x1_reverse", b11 ^ " " ^ a11 ^ " .",
   "-7.");
  ("simple_dot_1x1_size_error", a11 ^ " " ^ a21 ^ " .",
   "matrix size issue");
  ("simple_dot_1x1_size_reverse_error", a21 ^ " " ^ a11 ^ " .",
   "matrix size issue");
  ("simple_dot_1x2", "[[1.], [2.]] [[3.], [4.]] .",
   "11.");
   ("simple_dot_1x2_reverse", "[[3.], [4.]] [[1.], [2.]] .",
   "11.");
  ("simple_dot_1x3", a13 ^ " transpose " ^ b13 ^ " transpose .",
  "32.");
  ("simple_dot_1x3_reverse", b13 ^ " transpose " ^ a13 ^ " transpose .",
  "32.");
  ("simple_dot_3x3_size_error", a33 ^ " " ^ i33 ^ " .",
   "matrix size issue");
  (*simple int dot product*)
  ("simple_int_dot_prod", "[[2], [5], [4]] [[5], [3], [1]] .", "29");
  (*simple float dot product*)
  ("simple_float_dot_prod", "[[2.], [7.], [4.]] [[3.], [3.], [1.5]] .", "33.");

  (*-------- # --------*)
  ("simple_cross_3x1", "[[4., 5., 6.]] transpose [[7., 2., 3.]] transpose #",
  "[\n[ 3. ]\n[ 30. ]\n[ -27. ]\n]");
  ("simple_cross_3x1_reverse", "[[7., 2., 3.]] transpose [[4., 5., 6.]] transpose #",
  "[\n[ -3. ]\n[ -30. ]\n[ 27. ]\n]");
  (*simple cross product ints*)
  ("simple_int_cross_prod", "[[1], [2], [4]] [[6], [2], [1]] #", "[\n[ -6 ]\n[ 23 ]\n[ -10 ]\n]");
  (*simple cross product floatss*)
  ("simple_float_cross_prod", "[[6.5], [3.5], [4.]] [[2.], [4.], [10.]] #", "[\n[ 19. ]\n[ -57. ]\n[ 19. ]\n]");

  (*-------- = --------*)
  ("equal_a11", a11 ^ " " ^ a11 ^ " =",
   "1");
   ("equal_a21", a21 ^ " " ^ a21 ^ " =",
   "1");
   ("equal_a12", a12 ^ " " ^ a12 ^ " =",
   "1");
   ("equal_a22", a22 ^ " " ^ a22 ^ " =",
   "1");
   ("equal_b11", b11 ^ " " ^ b11 ^ " =",
   "1");
   ("equal_b22", b22 ^  " " ^ b22 ^ " =",
   "1");
   ("equal_c22", c22 ^  " " ^ c22 ^ " =",
   "1");
   ("equal_a13", a13 ^ " " ^ a13 ^ " =",
   "1");
   ("equal_b13", b13 ^ " " ^ b13 ^ " =",
   "1");
   ("equal_b12", b12 ^ " " ^ b12 ^ " =",
   "1");
   ("equal_i22", i22 ^ " " ^ i22 ^ " =",
   "1");
   ("equal_i33", i33 ^ " " ^ i33 ^ " =",
   "1");
   ("equal_a110", a110 ^ " " ^ a110 ^ " =",
   "1");
   ("equal_a220", a220 ^ " " ^ a220 ^ " =",
   "1");
   ("equal_a330", a330 ^ " " ^ a330 ^ " =",
   "1");
   ("equal_a231", a231 ^ " " ^ a231 ^ " =",
   "1");
   ("equal_a23", a23 ^ " " ^ a23 ^ " =",
   "1");

  ("equal_not_a11", a11 ^ " " ^ a22 ^ " =",
   "0");
   ("equal_not_a21", a21 ^ " " ^ a22 ^ " =",
   "0");
   ("equal_not_a12", a12 ^ " " ^ a22 ^ " =",
   "0");
   ("equal_not_b11", b11 ^ " " ^ a22 ^ " =",
   "0");
   ("equal_not_b22", b22 ^  " " ^ a22 ^ " =",
   "0");
   ("equal_not_c22", c22 ^  " " ^ a22 ^ " =",
   "0");
   ("equal_not_a13", a13 ^ " " ^ a22 ^ " =",
   "0");
   ("equal_not_b13", b13 ^ " " ^ a22 ^ " =",
   "0");
   ("equal_not_b12", b12 ^ " " ^ a22 ^ " =",
   "0");
   ("equal_not_i22", i22 ^ " " ^ a22 ^ " =",
   "0");
   ("equal_not_i33", i33 ^ " " ^ a22 ^ " =",
   "0");
   ("equal_not_a110", a110 ^ " " ^ a22 ^ " =",
   "0");
   ("equal_not_a220", a220 ^ " " ^ a22 ^ " =",
   "0");
   ("equal_not_a330", a330 ^ " " ^ a22 ^ " =",
   "0");
   ("equal_not_a231", a231 ^ " " ^ a22 ^ " =",
   "0");
   ("equal_not_a23", a23 ^ " " ^ a22 ^ " =",
   "0");


  (*-------- row --------*)
  ("row_1x1", a11 ^ " " ^ "0" ^ " row",
   "[\n[ 1. ]\n]");
  ("row_1x1_out_of_bounds", a11 ^ " " ^ "1" ^ " row",
   "index out of bounds");
  ("row_1x2", a12 ^ " " ^ "0" ^ " row",
  "[\n[ 2. -3. ]\n]");
  ("row_2x1", a21 ^ " " ^ "0" ^ " row",
  "[\n[ 2. ]\n]");
  ("row_2x2", a22 ^ " " ^ "1" ^ " row",
  "[\n[ -4. 5. ]\n]");
  ("row_3x3", a33 ^ " " ^ "2" ^ " row",
  "[\n[ 0. -5. 1. ]\n]");
  ("row_3x3_out_of_bounds", a33 ^ " " ^ "3" ^ " row",
  "index out of bounds");

  (*-------- col --------*)
  ("col_1x1", a11 ^ " " ^ "0" ^ " col",
   "[\n[ 1. ]\n]");
  ("col_1x1_out_of_bounds", a11 ^ " " ^ "1" ^ " col",
   "index out of bounds");
  ("col_1x2", a12 ^ " " ^ "0" ^ " col",
  "[\n[ 2. ]\n]");
  ("col_2x1", a21 ^ " " ^ "0" ^ " col",
  "[\n[ 2. ]\n[ 4. ]\n]");
  ("col_2x2", a22 ^ " " ^ "1" ^ " col",
  "[\n[ -3. ]\n[ 5. ]\n]");
  ("col_3x3", a33 ^ " " ^ "2" ^ " col",
  "[\n[ 5. ]\n[ 9. ]\n[ 1. ]\n]");
  ("col_3x3_out_of_bounds", a33 ^ " " ^ "3" ^ " col",
  "index out of bounds");

  (*-------- matrix_solve --------*)
  (*tests solving a simple system of eqn*)
  ("simple_solve",
   "[[2., -3.], [-4., 5.]] [[1.], [1.]] matrix_solve",
   "[\n[ -4. ]\n[ -3. ]\n]");
  ("solve_2x2_valid", i22 ^ " " ^ a21 ^ " matrix_solve",
  "[\n[ 2. ]\n[ 4. ]\n]");
  ("solve_2x2_invalid_reverse", a21 ^ " " ^ i22 ^ " matrix_solve",
  "matrix size issue");
  ("solve_2x2_valid_zero", i22 ^ " " ^ a210 ^ " matrix_solve",
  "[\n[ 0. ]\n[ 0. ]\n]");
  ("solve_2x2_wrong_height", i22 ^ " " ^ a31 ^ " matrix_solve",
  "matrix size issue");
  ("solve_3x3_valid", i33 ^ " " ^ a31 ^ " matrix_solve",
  "[\n[ 2. ]\n[ 4. ]\n[ 8. ]\n]");
  ("solve_3x3_invalid_reverse", a31 ^ " " ^ i33 ^ " matrix_solve",
  "matrix size issue");
  ("solve_3x3_valid_zero", i33 ^ " " ^ a310 ^ " matrix_solve",
  "[\n[ 0. ]\n[ 0. ]\n[ 0. ]\n]");
  ("solve_3x3_wrong_height", i33 ^ " " ^ a21 ^ " matrix_solve",
  "matrix size issue");
  ("solve_2x3_wrong_size", a23 ^ " " ^ a12 ^ " matrix_solve",
  "matrix size issue");
  ("solve_1x1_valid", a11 ^ " " ^ b11 ^ " matrix_solve",
  "[\n[ -7. ]\n]");
  ("solve_1x1_valid_zero", a11 ^ " " ^ a110 ^ " matrix_solve",
  "[\n[ 0. ]\n]");
  ("solve_1x1_invalid_reverse", a110 ^ " " ^ a11 ^ " matrix_solve",
  "this system is not consistent");
  ("solve_2x2_inconsistent", "[[2., -3.], [4., -6.]] [[1.], [1.]] matrix_solve",
  "this system is not consistent");


  (*-------- inv --------*)
  ("inv_1x1", a11 ^ " inv",
   "[\n[ 1. ]\n]");
  ("inv_2x2", c22 ^ " inv",
   "[\n[ -2. 3. ]\n[ 3. -4. ]\n]");
  ("inv_invalid_2x1", a21 ^ " inv",
   "matrix size error");
  ("inv_invalid_1x2", a12 ^ " inv",
   "matrix size error");
  ("inv_3x3", c33 ^ " inv",
   "[\n[ -24. 18. 5. ]\n[ 20. -15. -4. ]\n[ -5. 4. 1. ]\n]");

  (*-------- transpose --------*)
  ("transpose_1x1", a11 ^ " transpose",
   "[\n[ 1. ]\n]");
  ("transpose_2x2", a22 ^ " transpose",
   "[\n[ 2. -4. ]\n[ -3. 5. ]\n]");
  ("transpose_2x1", a21 ^ " transpose",
   "[\n[ 2. 4. ]\n]");
  ("transpose_1x2", a12 ^ " transpose",
   "[\n[ 2. ]\n[ -3. ]\n]");
  ("transpose_3x3", a33 ^ " transpose",
   "[\n[ 6. 6. 0. ]\n[ 3. 2. -5. ]\n[ 5. 9. 1. ]\n]");

  (*-------- echelon --------*)
  (*simple row reduction*)
  ("simple_ech",
   "[[2., -3.], [-4., 5.]] echelon",
   "[\n[ 2. -3. ]\n[ 0. -1. ]\n]");
  (*simple row reduction for a small integer matrix*)
  ("simple_int_ech",
   "[[2, -3], [4, 6]] echelon",
   "[\n[ 2 -3 ]\n[ 0 12 ]\n]");
  (*simple row reduction with more row than col*)
  ("non_square_rr",
   "[[2., -3.], [-4., 5.], [8., 7.]] echelon",
   "[\n[ 2. -3. ]\n[ 0. -1. ]\n[ 0. 0. ]\n]");
  (*simple row red with more col than row*)
  ("non_square_rr2",
   "[[2., -3., -1., 2.], [-4., 7., 5., 16.], [9., -70.5, 8., 7.]] echelon",
   "[\n[ 2. -3. -1. 2. ]\n[ 0. 1. 3. 20. ]\n[ 0. 0. 183.5 1138. ]\n]");
  (*lin dep ex.*)
  ("lin_dep_red",
   "[[2., -4., 9.], [-4., 8., 14.], [8., -16., -3.]] echelon",
   "[\n[ 2. -4. 9. ]\n[ 0. 0. 32. ]\n[ 0. 0. 0. ]\n]");

  (*-------- reduce --------*)
  (*simple row reduction to reduced form*)
  ("simple_row_red",
   "[[2., -3.], [-4., 5.]] reduce",
   "[\n[ 1. 0. ]\n[ 0. 1. ]\n]");
  (*simple row reduction with more row than col to reduced form*)
  ("non_square_rr",
   "[[2., -3.], [-4., 5.], [8., 7.]] reduce",
   "[\n[ 1. 0. ]\n[ 0. 1. ]\n[ 0. 0. ]\n]");
  (*simple row red with more col than row to reduced form*)
  ("non_square_rr2",
   "[[2., -3., -1., 2.], [-4., 7., 5., 16.], [9., -70.5, 8., 7.]] reduce",
   "[\n[ 1. 0. 0. 6.19346049046 ]\n[ 0. 1. 0. 1.39509536785 ]\n[ 0. 0. 1. 6.20163487738 ]\n]");
  (*lin dep ex. to reduced form*)
  ("lin_dep_red",
   "[[2., -4., 9.], [-4., 8., 14.], [8., -16., -3.]] reduce",
   "[\n[ 1. -2. 0. ]\n[ 0. 0. 1. ]\n[ 0. 0. 0. ]\n]");

  (*-------- det --------*)
  ("det_1x1", a11 ^ " det",
   "1.");
  ("det_1x1_not_one", b11 ^ " det",
   "-7.");
  ("det_1x1_zero", a110 ^ " det",
   "0.");
  ("det_2x2", a22 ^ " det",
   "-2.");
  ("det_2x2_ID", i22 ^ " det",
   "1.");
  ("det_2x2_zero", a220 ^ " det",
   "0.");
  ("det_invalid_1x3", a13 ^ " det",
   "matrix size issue");
  ("det_invalid_3x1", a31 ^ " det",
  "matrix size issue");
  ("det_3x3", a33 ^ " det",
   "114.");
  ("det_3x3_ID", i33 ^ " det",
   "1.");
  ("det_3x3_zero", a330 ^ " det",
   "0.");

  (*-------- indep --------*)
  ("indep_a11", a11 ^ " indep",
   "1");
   ("indep_a21", a21 ^ " indep",
   "1");
   ("indep_a22", a22 ^ " indep",
   "1");
   ("indep_a12", a12 ^ " indep",
   "0");
   ("indep_b11", b11 ^ " indep",
   "1");
   ("indep_b22", b22 ^ " indep",
   "1");
   ("indep_c22", c22 ^ " indep",
   "1");
   ("indep_a13", a13 ^ " indep",
   "0");
   ("indep_b13", b13 ^ " indep",
   "0");
   ("indep_b12", b12 ^ " indep",
   "0");
   ("indep_i22", i22 ^ " indep",
   "1");
   ("indep_i33", i33 ^ " indep",
   "1");
   ("indep_a110", a110 ^ " indep",
   "0");
   ("indep_a220", a220 ^ " indep",
   "0");
   ("indep_a330", a330 ^ " indep",
   "0");
   ("indep_a231", a231 ^ " indep",
   "0");
   ("indep_a23", a23 ^ " indep",
   "0");

  (*-------- dep --------*)
  ("dep_a11", a11 ^ " dep",
   "0");
   ("dep_a21", a21 ^ " dep",
   "0");
   ("dep_a22", a22 ^ " dep",
   "0");
   ("dep_a12", a12 ^ " dep",
   "1");
   ("dep_b11", b11 ^ " dep",
   "0");
   ("dep_b22", b22 ^ " dep",
   "0");
   ("dep_c22", c22 ^ " dep",
   "0");
   ("dep_a13", a13 ^ " dep",
   "1");
   ("dep_b13", b13 ^ " dep",
   "1");
   ("dep_b12", b12 ^ " dep",
   "1");
   ("dep_i22", i22 ^ " dep",
   "0");
   ("dep_i33", i33 ^ " dep",
   "0");
   ("dep_a110", a110 ^ " dep",
   "1");
   ("dep_a220", a220 ^ " dep",
   "1");
   ("dep_a330", a330 ^ " dep",
   "1");
   ("dep_a231", a231 ^ " dep",
   "1");
   ("dep_a23", a23 ^ " dep",
   "1");

  (*-------- rank --------*)
  ("rank_a11", a11 ^ " rank",
   "1");
   ("rank_a21", a21 ^ " rank",
   "1");
   ("rank_a12", a12 ^ " rank",
   "1");
   ("rank_a22", a22 ^ " rank",
   "2");
   ("rank_b11", b11 ^ " rank",
   "1");
   ("rank_b22", b22 ^ " rank",
   "2");
   ("rank_c22", c22 ^ " rank",
   "2");
   ("rank_a13", a13 ^ " rank",
   "1");
   ("rank_b13", b13 ^ " rank",
   "1");
   ("rank_b12", b12 ^ " rank",
   "1");
   ("rank_i22", i22 ^ " rank",
   "2");
   ("rank_i33", i33 ^ " rank",
   "3");
   ("rank_a110", a110 ^ " rank",
   "0");
   ("rank_a220", a220 ^ " rank",
   "0");
   ("rank_a330", a330 ^ " rank",
   "0");
   ("rank_a231", a231 ^ " rank",
   "1");
   ("rank_a23", a23 ^ " rank",
   "2");

  (*-------- nullspace --------*)
  ("nullspace_a11", a11 ^ " nullspace",
   "[\n[ 0. ]\n]");
   ("nullspace_a21", a21 ^ " nullspace",
   "[\n[ 0. ]\n]");
   ("nullspace_a12", a12 ^ " nullspace",
   "[\n[ 1.5 ]\n[ 1. ]\n]");
   ("nullspace_a22", a22 ^ " nullspace",
   "[\n[ 0. ]\n[ 0. ]\n]");
   ("nullspace_b11", b11 ^ " nullspace",
   "[\n[ 0. ]\n]");
   ("nullspace_b22", b22 ^ " nullspace",
   "[\n[ 0. ]\n[ 0. ]\n]");
   ("nullspace_c22", c22 ^ " nullspace",
   "[\n[ 0. ]\n[ 0. ]\n]");
   ("nullspace_a13", a13 ^ " nullspace",
   "[\n[ -2. -3. ]\n[ 1. 0. ]\n[ 0. 1. ]\n]");
   ("nullspace_b13", b13 ^ " nullspace",
   "[\n[ -1.25 -1.5 ]\n[ 1. 0. ]\n[ 0. 1. ]\n]");
   ("nullspace_b12", b12 ^ " nullspace",
   "[\n[ -4.85714285714 ]\n[ 1. ]\n]");
   ("nullspace_i22", i22 ^ " nullspace",
   "[\n[ 0. ]\n[ 0. ]\n]");
   ("nullspace_i33", i33 ^ " nullspace",
   "[\n[ 0. ]\n[ 0. ]\n[ 0. ]\n]");
   ("nullspace_a110", a110 ^ " nullspace",
   "[\n[ 1. ]\n]");
   ("nullspace_a220", a220 ^ " nullspace",
   "[\n[ 1. 0. ]\n[ 0. 1. ]\n]");
   ("nullspace_a330", a330 ^ " nullspace",
   "[\n[ 1. 0. 0. ]\n[ 0. 1. 0. ]\n[ 0. 0. 1. ]\n]");
   ("nullspace_a231", a231 ^ " nullspace",
   "[\n[ -1. -1. ]\n[ 1. 0. ]\n[ 0. 1. ]\n]");
   ("nullspace_a23", a23 ^ " nullspace",
   "[\n[ -0.576923076923 ]\n[ 0.230769230769 ]\n[ 1. ]\n]");

  (*-------- colspace --------*)
  ("colspace_a11", a11 ^ " colspace",
   "[\n[ 1. ]\n]");
   ("colspace_a21", a21 ^ " colspace",
   "[\n[ 2. ]\n[ 4. ]\n]");
   ("colspace_a12", a12 ^ " colspace",
   "[\n[ 2. ]\n]");
   ("colspace_a22", a22 ^ " colspace",
   "[\n[ 2. -3. ]\n[ -4. 5. ]\n]");
   ("colspace_b11", b11 ^ " colspace",
   "[\n[ -7. ]\n]");
   ("colspace_b22", b22 ^ " colspace",
   "[\n[ 7. 34. ]\n[ 56. -19. ]\n]");
   ("colspace_c22", c22 ^ " colspace",
   "[\n[ 4. 3. ]\n[ 3. 2. ]\n]");
   ("colspace_a13", a13 ^ " colspace",
   "[\n[ 1. ]\n]");
   ("colspace_b13", b13 ^ " colspace",
   "[\n[ 4. ]\n]");
   ("colspace_b12", b12 ^ " colspace",
   "[\n[ 7. ]\n]");
   ("colspace_i22", i22 ^ " colspace",
   "[\n[ 1. 0. ]\n[ 0. 1. ]\n]");
   ("colspace_i33", i33 ^ " colspace",
   "[\n[ 1. 0. 0. ]\n[ 0. 1. 0. ]\n[ 0. 0. 1. ]\n]");
   ("colspace_a110", a110 ^ " colspace",
   "[\n[ ]\n]");
   ("colspace_a220", a220 ^ " colspace",
   "[\n[ ]\n[ ]\n]");
   ("colspace_a330", a330 ^ " colspace",
   "[\n[ ]\n[ ]\n[ ]\n]");
   ("colspace_a231", a231 ^ " colspace",
   "[\n[ 1. ]\n[ 1. ]\n]");
   ("colspace_a23", a23 ^ " colspace",
   "[\n[ 6. 2. ]\n[ -2. -5. ]\n]");
]


let rsa_arith_tests = [
  (*tests generating public key from a private key computed beforehand*)
  ("gen_public_key","828508379315564229059901503743195890152288255550180775166444773564370495849 46823044180172892277581086552993376467 49226809994174581742439410872943569091 public_key",
  "n: 2304949099206212918857206153784356881859586917658227064795189032785987981497 e: 1613767402931803709848268727346680063096854248011089402527081469813588614409");
  (*test cracking a message encrypted with a small public key*)
  ("crack_small_key", "436127747 600282101 183129281 crack", "hi!");
  (*tests decrypting message in small public key coresponding to the
    given private key*)
  ("decrypt_small_key", "436127747 332532521 24631 24371 decrypt", "hi!");
  (*Descripts "Hello World!" when using a larger key*)
  ("decrypt_large_key",
  "36811825527290006912528373474500742165121558710088755054352605829758001844521 49865981988111168073411299457568574845326435683397292992624028577888245241871 217732754162686778772189526558422014371 271814926074039197007457045329062122427 decrypt",
  "Hello World!");
  (*Decripts A string ao ascii characters that are not all characters*)
  ("decrypt_strange",
  "4681768394463021453743047398362594156618121502944021046860601989734498485 311296661144438380085479553025783322441168863090489376579001194531304074481 71131826786915666159699380046190925721 43255453359216288668744181576802303777 decrypt",
  "{}!  #$)*")
]


let tests = [
  simple_lang_tests;
  simpl_arith_tests;
  mod_arith_tests;
  comb_arith_tests;
  linear_arith_tests;
  rsa_arith_tests;
]

let rec cont_eval l (v,env) =
  match l with
  | [] -> v
  | h :: t ->
      cont_eval t (Eval.evaluate_line (env) h )


let make_multi_line_tests =
  List.rev_map
    (fun (name, test, value) ->
      name >:: (fun _ -> assert_equal value (cont_eval test ("",PMap.empty)))
    )

let make_tests =
  List.rev_map
    (fun (name, test, value) ->
      name >:: (fun _ -> assert_equal value
                        (fst (Eval.evaluate_line (PMap.empty) test)) )
    )

let _ = run_test_tt_main ("suite" >::: ((make_multi_line_tests lang_tests)
                                        @(make_tests (List.flatten (tests)))))
