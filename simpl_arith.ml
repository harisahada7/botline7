open Types
open Big_int

let reduce (a,b) =
  let N(I g) = Mod_arith.gcd a b in
  let num = div_big_int a g in
  let denom = div_big_int b g in
    Q(num, denom)

let add a b =
  let add_rat a b c d =
    N(reduce (add_big_int
      (mult_big_int a d) (mult_big_int b c),
      (mult_big_int b d) ))
  in
    match a, b with
    | I(a), I(b) -> N(I(add_big_int a b))
    | F(a), F(b) -> N(F(a+.b))
    | Q(a, b), Q(c, d) -> add_rat a b c d
    | Q(a, b), I(c) -> add_rat a b c (unit_big_int)
    | I(a), Q(c, d) -> add_rat a (unit_big_int) c d
    | _ -> E("Incorrect Types")

let subtract a b =
  let sub_rat a b c d=
    N(reduce (sub_big_int
      (mult_big_int a d) (mult_big_int b c),
       mult_big_int b d))
  in
    match a, b with
    | I(a), I(b) -> N(I(sub_big_int a b))
    | F(a), F(b) -> N(F(a-.b))
    | Q(a, b), Q(c, d) -> sub_rat a b c d
    | Q(a, b), I(c) -> sub_rat a b c (unit_big_int)
    | I(a), Q(c, d) -> sub_rat a (unit_big_int) c d
    | _ -> E("Incorrect Types")

let multiply a b =
  match a, b with
  | I(a), I(b) -> N(I(mult_big_int a b))
  | F(a), F(b) -> N(F(a*.b))
  | Q(a, b), Q(c, d) ->  N(reduce ((mult_big_int a c),(mult_big_int b d)))
  | Q(a, b), I(c) -> N(reduce ((mult_big_int a c),b))
  | I(a), Q(c, d) -> N(reduce ((mult_big_int a c),d))
  | _ -> E("Incorrect Types")

let divide a b =
  try
    match a, b with
    | I(a), I(b) -> N(I(div_big_int a b))
    | F(a), F(b) -> N(F(a/.b))
    | Q(a, b), I(c) -> N(reduce (a,(mult_big_int b c)))
    | I(a), Q(b, c) -> N(reduce ((mult_big_int a c),b))
    | a, Q(c, d) ->  multiply a (Q (d, c))
    | _ -> E("Incorrect Types")
  with Division_by_zero -> E("division by 0")

let modulus a b = N(I((int_of_big_int a) mod (int_of_big_int b) |> big_int_of_int))

let power a b =
    match a, b with
    | I(a), I(b) ->
      if eq_big_int a zero_big_int = true && eq_big_int b zero_big_int
      then E "undefined"
      else N(I(power_big_int_positive_big_int a b))
    | F(a), F(b) ->
      if a = 0. && b = 0. then E("undefined")
      else N(F(a**b))
    | Q(a,b), Q(c,d) -> E("not supported")
    | _ -> E("Incorrect Types")

let eq a b =
  match a, b with
  | I(a), I(b) -> N(I (big_int_of_int (if compare_big_int a b = 0 then 1 else 0)))
  | F(a), F(b) -> N(I(big_int_of_int (if compare a b = 0 then 1 else 0)))
  | Q(a,b), Q(c,d) -> begin
     let Q(a', b') = reduce (a,b) in
     let Q(c', d') = reduce (c,d) in
       N(I (big_int_of_int (if compare_big_int a' b' = 0 then if compare_big_int c' d' = 0 then 1 else 0 else 0)))
  end
  | _ -> E("Incorrect types")
