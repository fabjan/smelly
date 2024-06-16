datatype ('a, 'b) result = Ok of 'a | Error of 'b

fun eq a b = a = b

fun fromOpt a NONE = a
  | fromOpt _ (SOME b) = b
