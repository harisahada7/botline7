open Types
open Big_int
open Simpl_arith

(**[number_from_value v] is the Types.integer option of the Types.value
 * [v]. If v is not a N of I of integer, None is returned
 *)
let big_int_of_value v : Types.integer option =
  match v with
  | N(I(intgr)) -> Some intgr
  | _ -> None

let factorial i =
  if sign_big_int i = -1 then
    E("Negative values are not allowed")
  else
    let rec factorial_helper accum i =
      if sign_big_int i = 0 then
        accum
      else
        factorial_helper (mult_big_int i accum) (pred_big_int i)
    in
    N(I(factorial_helper (big_int_of_int 1) i))

let combination n k =
  if sign_big_int n = -1 || sign_big_int k = -1 then
    E( "Negative values are not allowed")
  else
    let numerator_option = factorial n |> big_int_of_value in
    let leftdenom_option = factorial k |> big_int_of_value in
    let rightdenom_option = sub_big_int n k |> factorial |> big_int_of_value in
    match numerator_option, leftdenom_option, rightdenom_option with
    | Some numerator, Some ldenom, Some rdenom ->
      let denom = mult_big_int ldenom rdenom in
      N(I(div_big_int numerator denom))
    | _ -> E("Invalid inputs")

let partition_identical n k =
  combination
    ( sub_big_int (add_big_int n k) (big_int_of_int 1) )
    ( sub_big_int (k) (big_int_of_int 1) )

let permutation n k =
  if sign_big_int n = -1 || sign_big_int k = -1 then
    E("Negative values are not allowed")
  else
    let numerator_big_int_option = factorial n |> big_int_of_value in
    let denom_big_int_option = sub_big_int n k |> factorial |> big_int_of_value in
    match numerator_big_int_option, denom_big_int_option with
    | Some numerator, Some denom -> N(I(div_big_int numerator denom))
    | _ -> E("Invalid inputs")
