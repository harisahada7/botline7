open Types
(*[promt_and_read ()] will promt the user to input text and then read that
 * back in and return the resulting line in the form of a string*)
let promt_and_read _ =
  print_string "\n>";
  match read_line () with
  | exception End_of_file -> ""
  | line -> line

(*[display_output s] will print out the string s to the user*)
let display_output s = print_string s


(*[loop env] continually loops allowing the user to give input evaluating
 * and displaying the result*)
let rec loop env =
  let s = promt_and_read () in
  let (result, env') = Eval.evaluate_line env s in
    display_output result; loop env'


let init_enviro =
  PMap.empty
  |> PMap.add "'prime" (E "'prime has not be not bound")
  |> PMap.add "'p" (E "'p has not be not bound")
  |> PMap.add "'q" (E "'q has not be not bound")
  |> PMap.add "'n" (E "'n has not be not bound")
  |> PMap.add "'d" (E "'d has not be not bound")
  |> PMap.add "'e" (E "'e has not be not bound")
  |> PMap.add "'prime_prob" (E "'prime_prob has not be not bound")


(* [main ()] begins the repl which will continuously promt the user to input
 * commands for the calculator, evaluating them and then printining their result
 *)
let main _ =
  (print_string ("\n\n**Welcome to the CS helper buddy**"^
              "\nWe can help with, combinatorics, number theory,"^
              " and linear algebra, you are also welcome to make"^
              " your own macros to extend the functionality");
  loop (init_enviro))



let () = main ()