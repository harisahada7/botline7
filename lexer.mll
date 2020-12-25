{

open Types
open Big_int

let stack = Stack.create ()

(*[no_op op] matches the [op] with the operators which do not take any arguments
 * then evaluating the function coresponding to the operator
 * if the stack does not contain at least 2 elements then this evaluates to a
 * exception value indicating the wrong number of arguments, if the operator
 * has not been defined then this will evaluate to an exception value
 * also any of the argument exception values will be propagated, the earliest
 * of which takes precidence*)
let no_op env op =
  match op with
  | "gen_priv_key" -> begin
    let PrivKey(d, p, q) = Rsa.gen_private_key () in
      (PrivKey(d, p, q),
       env |> PMap.add "'p" (N (I p)) |> PMap.add "'q" (N (I q))
           |> PMap.add "'d" (N (I  d)) )
  end
  | "'prime"
  | "'p"
  | "'q"
  | "'n"
  | "'d"
  | "'e"
  | "'prime_prob" -> PMap.find op env, env
  | _ -> E("not a defined operator"), env

(*[un_op op] matches the [op] with the unerary operators, and the top element
 * on the stack with the legal form for the argument for that
 * operator, then evaluating the function coresponding to the operator
 * if the stack does not contain at least 2 elements then this evaluates to a
 * exception value indicating the wrong number of arguments, if the operator
 * has not been defined then this will evaluate to an exception value
 * also any of the argument exception values will be propagated, the earliest
 * of which takes precidence*)
let un_op env op =
  if Stack.length stack < 1 then E("wrong number of arguments"), env else
    let one = Stack.pop stack in
      match op, one with
      | "inv" , M(m) -> Linear_alg.inverse m, env
      | "transpose", M(m) -> Linear_alg.transpose m, env
      | "echelon", M(m) -> Linear_alg.row_echelon m, env
      | "reduce", M(m) -> Linear_alg.red_row_echelon m, env
      | "det", M(m) -> Linear_alg.determinant m, env
      | "indep", M(m) -> Linear_alg.lin_ind m, env
      | "dep", M(m) -> Linear_alg.lin_dep m, env
      | "rank", M(m) -> Linear_alg.rank m, env
      | "nullspace", M(m) -> Linear_alg.null_space m, env
      | "colspace", M(m) -> Linear_alg.col_space m, env
      | "!", N(I(i)) -> Comb_eval.factorial i, env
      | "factor", N(I(i)) -> Mod_arith.factor i, env
      | "gen_prime", N(I(i)) -> begin
         let prime = Mod_arith.gen_prime i in
           (prime, env |> PMap.add "'prime" prime)
      end
      | "is_prime", N(I(i)) -> Mod_arith.is_prime i, env
      | "is_prime_prob", N(I(i)) -> begin
        let is_prime = Mod_arith.is_prime_likely i in
          (is_prime, env |> PMap.add "'prime_prob" is_prime)
      end
      | "totient", N(I(i)) -> Mod_arith.totient i, env
      (*these operators are not actually un_ops, but they need to be able to
       * take the results of corresponding methods*)
      | "public_key", PrivKey(d, p, q) -> begin
          let PubKey(n, e) = Rsa.get_public_key (d,p,q) in
            (PubKey(n,e), env |> PMap.add "'n" (N (I n))
              |> PMap.add "'e" (N(I e)))
        end
      | _, E(e) -> E(e), env
      | _ -> E("not a defined operator"), env

(*[bin_op] matches the [op] with the binary operators, and the top 2 elements
 * on the stack with the legal forms for the arguments for that
 * operator, then evaluating the function coresponding to the operator
 * if the stack does not contain at least 2 elements then this evaluates to a
 * exception value indicating the wrong number of arguments, if the operator
 * has not been defined then this will evaluate to an exception value
 * also any of the argument exception values will be propagated, the earliest
 * of which takes precidence*)
let bin_op op =
  if Stack.length stack < 2 then E("wrong number of arguments for bin op") else
    let one = Stack.pop stack in
    let two = Stack.pop stack in
      match op, one, two with
      | "+", N(n1), N(n2) -> Simpl_arith.add n2 n1
      | "+", M(m1), M(m2) -> Linear_alg.add m2 m1
      | "-", N(n1), N(n2) -> Simpl_arith.subtract n2 n1
      | "-", M(m1), M(m2) -> Linear_alg.subtract m2 m1
      | "*", N(n1), N(n2) -> Simpl_arith.multiply n2 n1
      | "scale", M(m1), N(n2) -> Linear_alg.scale m1 n2
      | ".", M(m1), M(m2) -> Linear_alg.dot_product m2 m1
      | "#", M(m1), M(m2) -> Linear_alg.cross_product m2 m1
      | "/", N(n1), N(n2) -> Simpl_arith.divide n2 n1
      | "^", N(n1), N(n2) -> Simpl_arith.power n2 n1
      | "%", N(I(i1)), N((I i2)) -> Simpl_arith.modulus i2 i1
      | "=", N(n1), N(n2) -> Simpl_arith.eq n2 n1
      | "=", M(m1), M(m2) -> Linear_alg.eq m2 m1
      | "gcd", N(I(i1)), N((I i2)) -> Mod_arith.gcd i2 i1
      | "lcm", N(I(i1)), N((I i2)) -> Mod_arith.lcm i2 i1
      | "square", N(I(i1)), N((I i2)) -> Mod_arith.is_square i2 i1
      | "choose", N(I(i1)), N((I i2)) -> Comb_eval.combination i2 i1
      | "perm", N(I(i1)), N((I i2)) -> Comb_eval.permutation i2 i1
      | "part", N(I(i1)), N((I i2)) -> Comb_eval.partition_identical i2 i1
      | "row", N(I(i)), M(m) -> Linear_alg.row m i
      | "col", N(I(i)), M(m) -> Linear_alg.col m i
      | "matrix_solve", M(m1), M(m2) -> Linear_alg.solve m2 m1
      | "encrypt", PubKey(n,e), S(s) -> Rsa.encrypt (n,e) s
      | "decrypt", PrivKey(d,p,q), N(I(i)) -> Rsa.decrypt (d,p,q) i
      | "crack", PubKey(n,e), N(I(i)) -> Rsa.crack (n,e) i
      | _, _, E(e) -> E(e)
      | _, E(e), _ -> E(e)
      | _ -> E("not a defined bin operator")

(*[tri_op op] matches the [op] with the tirnary operators, and the top 3 elements
 * on the stack with the legal forms for the arguments for that
 * operator, then evaluating the function coresponding to the operator
 * if the stack does not contain at least 3 elements then this evaluates to a
 * exception value indicating the wrong number of arguments, if the operator
 * has not been defined then this will evaluate to an exception value
 * also any of the argument exception values will be propagated, the earliest
 *)
let tri_op env op =
  if Stack.length stack < 3 then
    if Stack.length stack > 0 && op = "public_key" then un_op env op
    else if Stack.length stack > 1 && op = "encrypt" then bin_op op, env
    else if Stack.length stack > 1 && op = "crack" then bin_op op, env
    else E("wrong number of arguments for tri op"), env
  else
      let one = Stack.pop stack in
      let two = Stack.pop stack in
      let three = Stack.pop stack in
        match op, one, two, three with
        | "?", n1, n2, N(n3) -> begin
          (match n3 with
          | I i -> if compare_big_int i zero_big_int = 0 then n1 else n2
          | F f -> if f = 0. then n1 else n2), env
        end
        | "+~",  N(I(i1)), N((I i2)), N(I(i3)) -> Mod_arith.add i3 i2 i1, env
        | "-~",  N(I(i1)), N((I i2)), N(I(i3)) -> Mod_arith.subtract i3 i2 i1, env
        | "*~",  N(I(i1)), N((I i2)), N(I(i3)) -> Mod_arith.multiply i3 i2 i1, env
        | "/~",  N(I(i1)), N((I i2)), N(I(i3)) -> Mod_arith.divide i3 i2 i1, env
        | "^~",  N(I(i1)), N((I i2)), N(I(i3)) -> Mod_arith.power i3 i2 i1, env
        | "=~",  N(I(i1)), N((I i2)), N(I(i3)) -> Mod_arith.eq i3 i2 i1, env
        | "bezout",  N(I(i1)), N((I i2)), N(I(i3)) -> Mod_arith.bezout i3 i2 i1, env
        | "crack",  N(I(e)), N((I n)), N(I(c)) -> Rsa.crack (n,e) c, env
        | "public_key", N(I(q)), N(I(p)), N(I(d)) -> begin
          let PubKey(n, e) = Rsa.get_public_key (d,p,q) in
            (PubKey(n,e), env |> PMap.add "'n" (N(I n)) |> PMap.add "'e" (N(I e)) )
        end
        | "encrypt", N(I(e)), N(I(n)), S(s) -> Rsa.encrypt (n,e) s, env
        | _, _,_,E(e) -> E(e), env
        | _, _,E(e),_ -> E(e), env
        | _, E(e),_,_ -> E(e), env
        | _ -> E("not a defined operator"), env

(*[quad_op op] matches the [op] with the quad operators, and the top 4 elements
 * on the stack with the legal forms for the arguments for that
 * operator, then evaluating the function coresponding to the operator
 * if the stack does not contain at least 4 elements then this evaluates to a
 * Exception value indicating the wrong number of arguments, if the operator
 * has not been defined then this will evaluate to an exception value
 * also any of the argument exception values will be propagated*)
let quad_op op =
  if Stack.length stack < 4 then
    if Stack.length stack > 1 && op = "decrypt" then bin_op op
    else E("wrong number of arguments")
  else
    let one = Stack.pop stack in
    let two = Stack.pop stack in
    let three = Stack.pop stack in
    let four = Stack.pop stack in
      match op, one, two, three, four with
      | "decrypt", N(I(q)), N(I(p)), N(I(d)), N(I(i)) -> Rsa.decrypt (d,p,q) i
      | _, _,_,_,E(e) -> E(e)
      | _, _,_,E(e),_ -> E(e)
      | _, _,E(e),_,_ -> E(e)
      | _, E(e),_,_,_ -> E(e)
      | _ -> E("not a defined quad operator")

(*[get_n n] gets [n] elements off of the stack returning them in the same order
 * which they are on the stack (top element on the stack is the first element
 * in the list)*)
let rec get_n n =
  if Stack.is_empty stack || n = 0 then [] else
    (Stack.pop stack)::(get_n (n-1))

(*[pair l] makes a pair of lists from [l] in which the odd elements are in
 * the first list and the even elements are in the second list where odd and
 * even refer to the ordering of the numbers not the values of the elements *)
let rec pair = function
  | [] -> ([],[])
  | (N(I a))::(N (I n))::t -> begin
    let (a', n') = pair t in
      (a::a', n::n')
  end

(*[apply f lst] applys the function [f] to a list of the values for wich x is
 * congrent to, and then a list of the modulos for those numbers
 * if any of the values is an exception value then this evaluates to the first
 * exn in [lst] *)
let apply f lst =
  let l = List.rev lst in
    if List.exists (fun v -> match v with | E(_) -> true | _ -> false) l then
       List.find (fun v -> match v with | E(_) -> true | _ -> false) l
    else
      let (a,n) = pair l in
        f a n

(*[multi_op op] takes an op, then looking at the stack to see how many arguments
 * will be given, then applys the operation to that number of arguments on the
 * stack.
 * if the number if arguments given is less than the number idicated, or
 * no such number is indicated then this
 * evaluares to an Exception value for "wrong number of arguments", if the
 * function called is not one of the listed multi ops then this evaluates
 * to an Excpetion value witht the text "not a defined operator"*)
let multi_op op =
  if Stack.length stack < 1 then E("wrong number of arguments") else
    let N(I(n)) = Stack.pop stack in
    let a = get_n (2* (Big_int.int_of_big_int n)) in
      match op with
      | "solve" -> apply (Mod_arith.crt) a
      | _ -> E("not a defined operator")

(*[row_to_list s f] converts the string form of a matrix to a list of number
 * types. The function f the string version of a single element to a number type
 *)
let rec row_to_list s f =
  let c = String.index_opt s ',' in
  let len = String.length s in
  match c with
    | None -> (f s)::[]
    | Some x ->
      (f (String.sub s 0 x))::(row_to_list (String.sub s (x+2) (len-x-2) ) f)

(*[make_rows s f] makes a list of lists wich are the rows of the matrix which
 * is represented as the string [s] using the function f to convert from a
 * string to a number tyoe *)
let rec make_rows s f =
  let beg = String.index_opt s '[' in
  let en = String.index_opt s ']' in
  let len = String.length s in
    match beg, en with
    | None , _ -> []
    | _, None -> []
    | Some b, Some e ->
      (((row_to_list (String.sub s (b+1) (e-b-1)) f))::
      (make_rows (String.sub s (e+1) (len-e-1)) f))


let check_row_len m =
  let len = Array.length m.(0) in
  if Array.fold_left (fun acc row -> acc && (Array.length row = len)) true m
  then M(m)
  else E("jaggad matricies are not allowed")

(*[make_matrix s f] gives the matrix in an array which is col major from
 * the string [s] using the function [f] to convert the string to an number type
 *)
let make_matrix s f =
  (*creates a list of the rows*)
  let list_m = (make_rows (String.sub s 1 ((String.length s) -2)) f) in
  (*creates the double array for the matrix*)
  let m = Array.make_matrix
    (List.length list_m)
    (List.length (List.hd list_m))
    (F(0.))
  in
    (*fills the rows of the matrix with the values*)
    List.iteri (fun i l -> m.(i) <- (Array.of_list l)) list_m; check_row_len m


}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let float =  '-'? digit+'.'digit*
let num = int | float
let letter = ['a'-'z' 'A'-'Z']
let vector = '[' num (", "num) * ']'
(*a matrix with any combination of numbers which contains a float will be
 * converted to a matrix of floats*)
let matrix = '[' vector (", "vector) * ']'
let int_vector = '[' int (", "int) * ']'
(*an int matrix can only contain integers not floats*)
let int_matrix = '[' int_vector (", "int_vector) * ']'
(*strings may contain any combination of charactures, but will not contain
 * any excapsed charactures*)
let string = '"' _ *  '"'
let id = (letter ) (letter | digit | "_")*
let nop = "gen_priv_key"
let uop = "inv" | "transpose" | "echelon" | "reduce" | "det" | "indep" | "dep"
          | "nullspace" | "colspace" | "!" | "factor" | "gen_prime"
          | "is_prime" | "is_prime_prob" | "totient" | "'prime" | "'p"
          | "'q" | "'n" | "'d" | "'e" | "'prime_prob" | "rank"
let bop = "+" | "-" | "*" | "/" | "^" | "%" | "=" | "gcd" | "lcm" | "square"
          | "choose" | "perm" | "part" | "." | "#" | "matrix_solve" | "row" |
           "col" | "scale"
let top = "?" | "+~" | "-~" | "*~" | "/~" | "^~" | "=~" | "crack" | "public_key"
          | "bezout" | "encrypt"
let qop = "decrypt"
let mop = "solve"
let op = id | nop | uop | bop | top | qop | mop


rule read env = parse
  | white { read env lexbuf }
  | string {
    (*this parses strings not function ids*)
    Stack.push (S (
      let s = Lexing.lexeme lexbuf in
        (*this removes the quotation marks from the string*)
        String.sub (s) 1 ((String.length s)-2 ) )
    ) stack;
    read env lexbuf
  }
  | op {
    let s = Lexing.lexeme lexbuf in
      if (PMap.mem s env) then
        match PMap.find s env with
        | Func (env', args, fun_string) -> begin
          if Stack.length stack < List.length args then
            (Stack.push (E "wrong number of arguments") stack; read env lexbuf)
          else
            let vals = List.rev (get_n (List.length args)) in
              (read
                (List.fold_left2 (fun m n v -> PMap.add n v m) env' args vals)
                (Lexing.from_string fun_string);
              read env lexbuf)
         end
        | v -> (Stack.push (v) stack; read env lexbuf)
      else
        let env' = baked_in env (Lexing.from_string (Lexing.lexeme lexbuf)) in
          read env' lexbuf
    }
  | int {
    Stack.push (N(I (Big_int.big_int_of_string (Lexing.lexeme lexbuf)))) stack;
    read env lexbuf
  }
  | float {
    Stack.push (N(F (float_of_string (Lexing.lexeme lexbuf)))) stack;
    read env lexbuf
  }
  | int_matrix {
    Stack.push (
      make_matrix
        (Lexing.lexeme lexbuf)
        (fun i -> (I(big_int_of_string i)))
    ) stack;
    read env lexbuf
  }
  | matrix {
    Stack.push (
      make_matrix
        (Lexing.lexeme lexbuf)
        (fun f -> (F(float_of_string f)))
     ) stack;
    read env lexbuf
  }

  | _ {
    Stack.push
      (E("I do not understand the token: "^(Lexing.lexeme lexbuf)))
      stack; read env lexbuf
  }
  | eof {env }
and baked_in env = parse
  | nop   {
    let value, env' = no_op env (Lexing.lexeme lexbuf) in
      Stack.push (value) stack; env'
  }
  | uop   {
    let value, env' = un_op env (Lexing.lexeme lexbuf) in
      Stack.push (value) stack; env'
  }
  | bop   {
    let value = bin_op (Lexing.lexeme lexbuf) in
    Stack.push (value) stack; env
  }
  | top   {
    let value, env' = tri_op env (Lexing.lexeme lexbuf) in
      Stack.push (value) stack; env'
  }
  | qop   {
    let value = quad_op (Lexing.lexeme lexbuf) in
      Stack.push (value) stack; env
  }
  | mop   {
    let value = multi_op (Lexing.lexeme lexbuf) in
      Stack.push (value) stack; env
  }
  | _     { Stack.push (E "not defined") stack; env}






