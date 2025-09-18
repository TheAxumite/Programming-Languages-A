(* Programming Languages, Dan Grossman *)
(* Section 3: Another Closure Idiom: Combining Functions *)

fun compose (f,g) = fn x => f(g,x)

val test = compose((fn (acc,x) => acc + x), 3) 2;
