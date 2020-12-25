open Types
open Big_int

(*[easy_totient p q] is the euler totient applied to n, where p*q = n
Precondtion: p,q>0*)
let easy_totient p q =
  let p' = pred_big_int p in
  let q' = pred_big_int q in
  mult_big_int p' q'

(*[gen_private_key ()] is a psuedorandomly generated 256 RSA private key
  with exceedingly high probability expesed as PrivKey(d,p,q)
  Note: the exceedingly high probability is a result of random prime
  generation using a highly accurate probabilistic prime test in Mod_arith*)
let gen_private_key _ =
  let half_key_size = big_int_of_int 128 in
  let p = Mod_arith.as_big_int (Mod_arith.gen_prime half_key_size) in
  let q = Mod_arith.as_big_int (Mod_arith.gen_prime half_key_size) in
  let n = mult_big_int p q in
  let phi = easy_totient p q in
  let d = Mod_arith.gen_unit phi in
  PrivKey (d,p,q)

(*[get_public_key (d,p,q)] is PubKey(n,e) where n = p*q
  and e is the multiplicative inverse of d mod phi(n),
  where phi is the euler totient function
  Precondtion: (d,p,q) is a valid RSA private key*)
let get_public_key (d,p,q) =
  let n = mult_big_int p q in
  let phi = easy_totient p q in
  let N(I(e)) = Mod_arith.inv d phi in
  (PubKey(n,e))

(*[explode s] is the list containing the characters in s, in the same order
  that the appear in s, or the empty list if s is the empty string
  Note: ocaml documents a similar verion of this function,
  but does not implement it in the standard library.
  This was found on stack overflow by user Ptival*)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(*[encode_char_as_big_int c] is the ascii code for c + 100 represented
  as a big_int*)
let encode_char_as_big_int c =
  let code = (Char.code c) + 100 in
  big_int_of_int code

(*[encode_string_as_big_int s] is the big_int representation of s as a big_int,
  a, where each character in s from left to right, appears as its coresponding
  3-digit ascii code + 100, from right to left in a*)
let encode_string_as_big_int s =
  let shift = big_int_of_int 1000 in
  let char_lst = explode s in
  let rec encode_lst_as_big_int l accum =
    match l with
    | [] -> accum
    | h::t ->
      let i = encode_char_as_big_int h in
      let accum' = mult_big_int accum shift in
      encode_lst_as_big_int t (add_big_int i accum') in
  encode_lst_as_big_int char_lst zero_big_int

(*[decode_big_int_as_char i] is the character in ascii respresented
  by code i -100
  Precondtion: 0 <= (i-100) < 128*)
let decode_big_int_as_char i =
  try Char.chr ((int_of_big_int i) - 100)
  with | Invalid_argument _ -> Char.chr 0

(*[decode_big_int_as_string i] is unique string s such that
  [encode_string_as_big_int i] = s *)
let decode_big_int_as_string i =
  let rec decode_big_int_as_lst i accum =
    if (eq_big_int i zero_big_int) then accum
    else let i' = div_big_int i (big_int_of_int 1000) in
      let r = mod_big_int i (big_int_of_int 1000) in
      decode_big_int_as_lst i' (Char.escaped (decode_big_int_as_char r)::accum)
  in
  String.concat "" (decode_big_int_as_lst i [])

(*[encrypt (n,e) s] is the string result of encrypting
  [encode_string_as_big_int s] using RSA public key (n,e)
  Precondition: (n,e) is a valid RSA public key,
  [encode_string_as_big_int s] < n, e [encode_string_as_big_int s] is
  relatively prime to n and s contains only ascii characters*)
let encrypt (n,e) s =
  let m = encode_string_as_big_int s in
  Mod_arith.power m e n

(*[decrypt (d,p,q) c] is s such that [encrypt (p*q,d^-1) s] is c, where d^-1
  is the multiplicative inverse of d (mod phi(p*q)), with phi, the euler phi function
  Precondtion: (d,p,q) is a valid RSA private key and c is a valid result of
  [encrypt (p*q,e) s], for some s*)
let decrypt (d, p, q) c =
  let n = mult_big_int p q in
  let m = Mod_arith.as_big_int (Mod_arith.power c d n) in
  S(decode_big_int_as_string m)

(*[crack (n,e) c] is s such that [encrypt (p*q,e) s] is c,
  where n = p*q for primes p and q
  Precondtion: (n,e) is a valid RSA public key and c is a valid result of
  [encrypt (n,e) s], for some s*)
let crack (n, e) c =
  let phi = Mod_arith.as_big_int (Mod_arith.totient n) in
  let d = Mod_arith.as_big_int (Mod_arith.inv e phi) in
  let m = Mod_arith.as_big_int (Mod_arith.power c d n) in
  S(decode_big_int_as_string m)
