
open Types
open Big_int

(*[init_matrix x y f] creates a 2D matrix with [x] rows and [y] cols
 * using the function [f] to get the value for each cell*)
let init_matrix x y f =
  Array.init x (fun i -> Array.init y (fun j ->  f i j))

(*[iterij f] iterates over a 2D array applying the function f to each of the
 * cells*)
let iterij f =
  Array.iteri (fun i row -> Array.iteri (fun j v -> f i j v) row)

(*[non_zero v] tests that [v] is not zero*)
let non_zero v =
  match v with
  | I(i) -> not (eq_big_int i zero_big_int)
  | F(f) -> not (f = 0.)
  | Q(a,b) -> not (eq_big_int a zero_big_int)

(*[zero v] gives a zero value which is of the same type as [v]*)
let zero v =
  match v with
  | I _ -> I(zero_big_int)
  | F _ -> F(0.0)
  | Q _ -> Q(zero_big_int, unit_big_int)

let row m n =
  try
    let i = int_of_big_int n in
      if i < 0 || i >= Array.length m then E("index out of bounds")
      else
        M(init_matrix 1 (Array.length m.(0)) (fun _ j -> m.(i).(j)))
  with Failure _ -> (E "index is to large")


let col m n =
  try
    let i = int_of_big_int n in
      if i < 0 || i >= Array.length m.(0) then E("index out of bounds")
      else
        M(init_matrix (Array.length m) 1 (fun i' _ -> m.(i').(i)))
  with Failure _ -> E("index is too large")



let dot_product m1 m2 =
  let l1 = Array.length m1 in
  let l2 = Array.length m2 in
    if (Array.length m1.(0)) <> 1 ||
      (Array.length m1.(0)) <> (Array.length m2.(0)) ||
      l1 <> l2 then E("matrix size issue")
    else
      let rec mult n sum =
        if n = l1 then sum else
          let N(prod) = Simpl_arith.multiply m1.(n).(0) m2.(n).(0) in
          let N(new_sum) = (Simpl_arith.add sum prod) in
            mult (n+1) new_sum
      in N(mult 0 (zero m1.(0).(0)))

let cross_product m1 m2 =
  let l1 = Array.length m1 in
  let l2 = Array.length m2 in
  let w1 = Array.length m1.(0) in
  let w2 = Array.length m2.(0) in
  let r = Array.make_matrix 3 1 (F(0.) ) in
    if w1 <> 1 || w2 <> 1 || l2 <> 3 || l1 <> 3 then E("matrix size issue")
    else
      let a1,a2,a3,b1,b2,b3 = m1.(0).(0), m1.(1).(0), m1.(2).(0), m2.(0).(0),
                              m2.(1).(0), m2.(2).(0) in
      let N(prod1) = Simpl_arith.multiply a2 b3 in
      let N(prod2) = Simpl_arith.multiply a3 b2 in
      let N(sum1) = Simpl_arith.subtract prod1 prod2 in
      let N(prod3) = Simpl_arith.multiply a3 b1 in
      let N(prod4) = Simpl_arith.multiply a1 b3 in
      let N(sum2) = Simpl_arith.subtract prod3 prod4 in
      let N(prod5) = Simpl_arith.multiply a1 b2 in
      let N(prod6) = Simpl_arith.multiply a2 b1 in
      let N(sum3) = Simpl_arith.subtract prod5 prod6 in
        r.(0).(0) <-  sum1;
        r.(1).(0) <-  sum2;
        r.(2).(0) <-  sum3;
        M(r)

let scale m n =
  M(Array.map
    (fun r -> Array.map
      (fun v ->
        let N(num) = (Simpl_arith.multiply n v) in
          num
      )r
    ) m )

let transpose m =
  let res = Array.make_matrix (Array.length m.(0)) (Array.length m) (F(0.) ) in
    Array.iteri (fun i r -> Array.iteri (fun j v -> res.(j).(i) <- v ) r ) m;
    M(res)

(*[simple_bin f m1 m2] applies the function [f] to the entries of the
 * matricies [m1] and [m2] to combine them*)
let simple_bin f m1 m2 =
  let n1 = Array.length m1 in
  let n2 = Array.length m2 in
  let o1 = Array.length (m1.(0)) in
  let o2 = Array.length (m2.(0)) in
  if n1 = n2 && o1 = o2 then
    M(Array.map2 (fun r1 r2 -> Array.map2 (f) r1 r2) m1 m2)
  else
    E("matrix size issue")

let add =
  simple_bin (fun a b -> let N(num) = Simpl_arith.add a b in num)

let subtract =
  simple_bin (fun a b -> let N(num) = Simpl_arith.subtract a b in num)

(*[clear_col j i1 i2] substracts a multiple of i1 from i2 such that the
 * leading entry of i2 will become 0(the leading entry is in the [j]
 * col)*)
let clear_col j i1 i2 =
  let N(p) = (Simpl_arith.divide  i2.(j) i1.(j) ) in
    Array.mapi
    (fun i v ->
       let N(mul) = (Simpl_arith.multiply i1.(i) p ) in
       let N(value) = (Simpl_arith.subtract v mul) in
         value
    ) i2


(*[fin_non_zero m i j] findes the first non_zero entry in the col [j] of
 * the matrix [m]*)
let rec find_non_zero m i j =
  let rows = Array.length m in
  let cols = Array.length (m.(0)) in
    if i < rows && j < cols then
      if non_zero m.(i).(j) then i else find_non_zero m (i+1) j
    else -1

(*[swap m i1 i2] swaps the [i1] and [i2] rows in the matrix [m]*)
let swap m i1 i2 =
  let temp = m.(i1) in
    m.(i1) <- m.(i2);
    m.(i2) <- temp

(*[convert_to_rat m] if the matrix is an integer matrix converts the matrix to
 * a rational matrix*)
let convert_to_rat m =
  match m.(0).(0) with
  | I _ -> (init_matrix (Array.length m) (Array.length m.(0))
    (fun i j -> let I(n) = m.(i).(j) in Q(n, big_int_of_int 1) ) )
  | F _ -> m
  | Q _ -> m

(*[red_row_down m i j] reduces the [m] into row reduced form where the matrix
 * row [i] and col [j] is already of that form that result is then paired with
 * the number of row swaps made though the process*)
let rec red_row_down m i j =
  let rows = Array.length m in
  let cols = Array.length (m.(0)) in
    if i < rows && j < cols then
      match m.(i).(j) with
      | (F 0.0) -> begin
        let non_zero = find_non_zero m (i+1) j in
          if non_zero = -1 then red_row_down m i (j+1)
          else
            let (rr,s) = (swap m i non_zero; red_row_down m i j) in
              (rr,s+1)
      end
      | Q (z, _ ) when (eq_big_int z (zero_big_int))-> begin
          let non_zero = find_non_zero m (i+1) j in
            if non_zero = -1 then red_row_down m i (j+1)
            else
              let (rr,s) = (swap m i non_zero; red_row_down m i j) in
                (rr,s+1)
      end
      | _ ->
        (Array.iteri
          (fun ind row ->
            if ind > i then
              (m.(ind) <- (clear_col j ( m.(i) ) row ))
            else ()
          ) m;
          red_row_down m (i+1) (j+1))
    else
     (M(m),0)

let row_echelon m =
  let m = convert_to_rat m in
   fst (red_row_down (Array.map (fun row -> Array.copy row) m) 0 0)

(*[find_pivot m i] finds the pivot (first non zero number) in the row [i]
 * of the matrix [m]*)
let find_pivot m i =
  let rec piv j =
    if j >= Array.length m.(i) then -1
    else if non_zero m.(i).(j) then j else piv (j+1)
  in piv i

(*[red_row_up m i] transforms the matrix from row echelon form to row
 * reduced form where [m] below the row [i] is alrady of that form*)
let rec red_row_up m i=
  if i >= 0 then
    let j = find_pivot m i in
      if j = -1 then red_row_up m (i-1) else
        let piv_val = m.(i).(j) in
          (Array.iteri (fun j' v ->
            let N(n) = Simpl_arith.divide v (piv_val) in
              m.(i).(j') <- n
          ) m.(i);
          Array.iteri (fun ind row ->
            if ind < i then m.(ind) <- (clear_col j ( m.(i) ) row ) else ()
          ) m;
          red_row_up m (i-1))
  else
    M(m)

let red_row_echelon m =
  let M(ech) = row_echelon m in
    red_row_up ech (Array.length ech -1)

(*[read_off_inv aug m] given the augmented matrix [aug] which has twice as many
 * col as [m] transfers the inverse of m into the matrix m it self which will be
 * the right square matrix in [aug]*)
let read_off_inv aug m =
  let cols = Array.length (m.(0)) in
    iterij (fun i j v -> m.(i).(j) <- aug.(i).(j+cols)) m

let inverse m =
  let rows = Array.length m in
  let cols = Array.length (m.(0)) in
    if rows = cols && rows <> 0 then
      let z,o =
        match m.(0).(0) with
        | I _ -> (I (big_int_of_int 0)),(I (big_int_of_int 1))
        | F _ -> (F 0.), F(1.)
      in
      let aug = (init_matrix rows (cols*2)
          (fun i j ->
            if j < cols then m.(i).(j)
            else if i = j-cols then o else z)
         ) in
        let M(solved) = red_row_echelon aug in
          (read_off_inv solved m; M(m))
    else
      E "matrix size error"

(*[prod_diag m i acc] takes the product of the entries along the primary
 * diagonal of [m] where the entries to the left and above [i] have already been
 * taken into account
 * requires: m has been reduced to row eschelon form *)
let rec prod_diag m i acc =
  let rows = Array.length m in
  let cols = Array.length (m.(0)) in
    if i < rows && i < cols then
      let N(prod) = Simpl_arith.multiply acc m.(i).(i) in
        prod_diag m (i+1) (prod)
    else acc

(*[unit v] gives a zero value which is of the same type as [v]*)
let reg_unit v =
  match v with
  | I _ -> I(unit_big_int)
  | F _ -> F(1.0)
  | Q _ -> Q(unit_big_int, unit_big_int)

(*[neg_unit v] gives a zero value which is of the same type as [v]*)
let neg_unit v =
  match v with
  | I _ -> I(minus_big_int unit_big_int)
  | F _ -> F(-1.0)
  | Q _ -> Q(minus_big_int unit_big_int, unit_big_int)

let determinant m =
  let rows = Array.length m in
  let cols = Array.length (m.(0)) in
  if rows = cols then
    let M(rr),swap_count = red_row_down m 0 0 in
      N(prod_diag m 0 (
        if swap_count mod 2 = 1 then neg_unit m.(0).(0)
        else reg_unit m.(0).(0)))
  else E("matrix size issue")

(*[piv_col m f g pinit ninit] essential traverses the pivots in a matrix
 * applys the function f to the row and col number of
 * the pivot col in the row eschelon matrix [m] using pinit for the initial
 * value, and applys g to every pivot row and col with nacc as its initial value
 * requires: m has been reduced to row echelon form*)
let piv_col m f g pinit ninit=
  let rows = Array.length m in
  let cols = Array.length (m.(0)) in
  let rr = row_echelon m in
    let rec trav_diag i j pacc nacc =
      if i < rows && j < cols then
        (*this is a pivot position*)
        if non_zero (m.(i).(j)) then
          trav_diag (i+1) (j+1) (f i j pacc) (g i j nacc)
        else trav_diag (i) (j+1) pacc nacc
      else (pacc, nacc)
    in trav_diag 0 0 pinit ninit

(*[negate v] negates the value [v]*)
let negate v =
  match v with
  | I(i) -> I (minus_big_int i)
  | F(f) -> F(-1. *. f)
  | Q(a, b) -> Q(minus_big_int a, b)

(*[rem v l] revoves the value v from the [l]*)
let rec rem v l =
  match l with
  | [] -> []
  | h::t -> if h = v then t else h::(rem v t)

(*[from i n acc] makes a list with values from i to n*)
let rec from n acc =
  if n < 0 then acc else (from (n-1) (n::acc))

let rec exists_non_zero n m =
  if n < 0 then false
  else if non_zero (m.(n)) then true
  else exists_non_zero (n-1) m

(*[check_consistent m i] chacks that the matrix m is consistent in the
 * row i and all those above*)
let rec check_consistent m i =
  if i < 0 then true else
    let cols = Array.length m.(0) in
      (* if the value on the RHS of the augmented matrix is non zero then there
       * must be a non zero value in the rest of the row, otherwise the system
       * is inconsistent*)
      if (non_zero m.(i).(cols-1)) then
        if exists_non_zero (cols-2) (m.(i)) then check_consistent m (i-1)
        else false
      else
        check_consistent m (i-1)

(*[read_off_sol m] for a matrix that is in augmented form and has one singular
 * solution this reads off the solution*)
let read_off_sol m =
  let cols = Array.length (m.(0)) in
  (*gives a list of piv positions and and non piv cols*)
  let (piv, non_piv) = piv_col m (fun i j acc -> (i,j)::acc)
                      (fun _ j acc -> rem j acc ) [] (from (cols-1) [])in
  let z,o = zero (m.(0).(0)), reg_unit (m.(0).(0)) in
  (*the result vecotrs have the same length as the number of col in the
   * original matrix and there are as many of them as there are free variable
   * also the same as the number of non_pivot cols*)
  let result = Array.make_matrix (cols-1) ((List.length non_piv)) z in
    (*for each pivot place the values for the coefficent for the free variables
     * in the solution matrix*)
    List.iter
      (fun (i,j) -> List.iteri
        (fun n ( j') ->
          if j' = cols-1 then result.(j).(n) <- (m.(i).(j'))
          else result.(j).(n) <- negate (m.(i).(j'))
        ) non_piv
      ) piv;
    (*for each of the free variables the row and col coresponging to a free
     *variable should have a one*)
    List.iteri
      (fun i (j) -> if j = cols-1 then () else result.(j).(i) <- o) non_piv;
    result

let solve m1 m2 =
  let z = zero (m1.(0).(0)) in
  let rows1 = Array.length m1 in
  let rows2 = Array.length m2 in
  let cols1 = Array.length m1.(0) in
  let cols2 = Array.length m2.(0) in
    if rows1 <> 0 && cols1 <> 0 && cols2 = 1 &&  rows1 = rows2 then
        let aug = Array.make_matrix rows1 (cols1+1) (F 0.) in
          (*adds m to the augmented matrix*)
          Array.iteri (fun i row -> Array.iteri (fun j v ->
            aug.(i).(j) <- v
          ) row) m1;
          (*adds vector to the augmented matrix*)
          Array.iteri (fun i row -> aug.(i).(cols1) <- row.(0)) m2;
            let M(sol) = red_row_echelon aug in
              if check_consistent sol (rows1 -1)
              then M(read_off_sol sol)
              else E("this system is not consistent")
    else
      E ("matrix size issue")

let rank m =
  let M(rr) = row_echelon m in
  (*counts the number of pivots*)
  let (r,_) = piv_col rr (fun i j acc -> 1 + acc)
                         (fun _ _ _ -> ()) 0 () in
    N(I(big_int_of_int r))

let lin_ind m =
  let N(I(r)) = rank m in
    if eq_big_int r (big_int_of_int (Array.length m.(0)) )
    then N (I (unit_big_int))
    else N (I (zero_big_int))

let lin_dep m =
  let N(I(r)) = rank m in
    if eq_big_int r (big_int_of_int (Array.length m.(0)) )
    then N (I (zero_big_int))
    else N (I (unit_big_int))

let null_space m =
  let z = zero (m.(0).(0)) in
  (*solves the linear system with the right side of the augmented matrix being
   *all zeros (matrix with the same number of rows and i col)*)
  match solve m (Array.make_matrix (Array.length m) (1) (z)) with
  | M(arr) -> begin
    (*if the null space has more than one vector then the last vector will be
     * a zero vector since the right side of the augmented matrix started at all
     * zeros*)
    if (Array.length arr.(0)) > 1 then
      (*removes the last col*)
      M(init_matrix
        (Array.length arr)
        ((Array.length arr.(0)) - 1)
        (fun i j -> arr.(i).(j)))
    else
      M(arr)
  end
  (*an inconsistent system should only contain the zero vector*)
  | E(msg) when (msg = "this system is not consistent") ->
      M(Array.make_matrix (Array.length m.(0)) (1) (z))
  | x -> x

let col_space m =
  let M(rr) = row_echelon m in
  let piv_list = fst (piv_col rr (fun _ j acc -> j::acc) (fun _ _ _ -> ()) [] () ) in

  let piv = Array.of_list (List.rev piv_list) in
    M(init_matrix (Array.length rr) (Array.length piv) (fun i j -> m.(i).(piv.(j))))

let eq m1 m2 =
  let r1 = Array.length m1 in
  let c1 = Array.length (m1.(0)) in
  let r2 = Array.length m2 in
  let c2 = Array.length (m2.(0)) in
    if r1 = r2 && c1 = c2 then
      let e = ref 1 in
        Array.iter2 (fun r1 r2 -> Array.iter2 (fun v1 v2 -> if v1 = v2 then () else e := !e * 0) r1 r2) m1 m2;
        N(I(big_int_of_int (!e)))
    else
      N(I(big_int_of_int (0)))
