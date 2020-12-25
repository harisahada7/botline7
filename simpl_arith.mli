(*simple arithmetic compuations like adding/multiply/power/divide *)

(*[add a b] is a + b if a and b are both integers, a +. b if a and b are
  both floats
  Precondition: a,b are both either integers or floats, and are the same type,
  if they are not the result will be an excpetion value*)
val add: Types.number -> Types.number -> Types.value

(*[subtract a b] is a - b if a and b are both integers, a -. b if a and b are
  both floats
  Precondition: a,b are both either integers or floats, and are the same type
  if they are not the result will be an excpetion value*)
val subtract: Types.number -> Types.number -> Types.value

(*[multiply a b] is a * b if a and b are both integers, a *. b if a and b are
  both floats
  Precondition: a,b are both either integers or floats, and are the same type
  if they are not the result will be an excpetion value*)
val multiply: Types.number -> Types.number -> Types.value

(*[divide a b] is a / b if a and b are both integers, a /. b if a and b are
  both floats
  Precondition: a,b are both either integers or floats, a,b are the same type
  and b != 0, if they are not the result will be an excpetion value*)
val divide: Types.number -> Types.number -> Types.value

(*[modulus a b] is a mod b if a and b are both integers
  Precondition: a,b are both either integers or floats, a,b are the same type
  and b != 0, if they are not the result will be an excpetion value*)
val modulus: Types.integer -> Types.integer -> Types.value

(*[power a b] is a^b, warning on floats this value may overflow*)
val power: Types.number -> Types.number -> Types.value

(*[eq a b] returns a non zero value if [a] [b] are the same value and
 * a zero value if they are not equal*)
val eq: Types.number -> Types.number -> Types.value