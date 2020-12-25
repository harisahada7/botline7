(* linear algebra compuations like dot product, cross product, etc. *)

(* [row a x] returns the row of [a] at index [x]
 * if [x] is not an the index of a row in [a], this
 * evaluates to an exeption value*)
val row: Types.matrix -> Types.integer -> Types.value

(* [col a x] returns the column of [a] at index [x]
 * if [x] is not an index of a column in [a], this
 * evaluates to an exeption value*)
val col: Types.matrix -> Types.integer -> Types.value

(* [dot_product a b] is [a]•[b]
 * if the width of [a] is not the same as height of [b] then
 * this evaluates to an exeption value*)
val dot_product : Types.matrix -> Types.matrix -> Types.value

(* [cross_product a b] is [a]x[b]
 * if [a] and [b] are not three dimensional vectors then this evaluates
 * to an exeption value*)
val cross_product : Types.matrix -> Types.matrix -> Types.value

(* [scale a b] is the [a] scaled by factor [b] *)
val scale: Types.matrix -> Types.number -> Types.value

(* [inverse a] is the inverse matrix of [a]
 * if [a] is not an invertable a square matrix, then this evaluates
 * to an exception value*)
val inverse: Types.matrix -> Types.value

(* [transpose a] is [a] transposed *)
val transpose: Types.matrix -> Types.value

(* [add a b] is the sum of [a] and [b]
 * if [a] dees not have the same shape as [b] then this evaluates
 * to an exeption value *)
val add: Types.matrix -> Types.matrix -> Types.value

(* [add a b] is the difference of [a] and [b]
 * if [a] does not have the same shape as [b] then this
 * evaluates to an exeption value*)
val subtract: Types.matrix -> Types.matrix -> Types.value

(* [row_echelon a] is [a] transformed into row echelon form*)
val row_echelon: Types.matrix -> Types.value

(* [red_row_echelon a] is [a] transformed into reduced row echelon form*)
val red_row_echelon: Types.matrix -> Types.value

(* [solve a b] is the matrix containing the solution to the equation [A]x=[b]
 * where [a] is the coefficients for a system of equations
 * and b is a vector containing constants
 * if [a] does not have same height as [b] or the system is inconsistent, or
* there are infinitely many solutions this evaluates to an exception value *)
val solve: Types.matrix -> Types.matrix -> Types.value

(* [determinant a] is the determinant of matrix [a]
 * if [a] is not a square matrix this evaluates to an exception value*)
val determinant: Types.matrix -> Types.value

(*[rank m] calculates the rank of the matrix [m]*)
val rank: Types.matrix -> Types.value

(* [lind_ind a] returns a non zero value if [a] is independent and
 * a zero value if it is not *)
val lin_ind: Types.matrix -> Types.value

(* [lind_dep a] returns a non zero value if [a] is dependent and
 * a zero value if it is not *)
val lin_dep: Types.matrix -> Types.value

(* [null_space a] is the matrix containing the basis for the null space of [a] *)
val null_space: Types.matrix -> Types.value

(* [null_space a] is the matrix containing the basis for the col space of [a] *)
val col_space: Types.matrix -> Types.value

(*[eq a b] returns a non zero value if [a] [b] are the same matrix and
 * a zero value if they are not*)
val eq: Types.matrix -> Types.matrix -> Types.value
