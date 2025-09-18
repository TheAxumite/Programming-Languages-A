(* Programming Languages, Dan Grossman *)
(* Section 3: Map and Filter *)

(* here is a very, very useful and common example *)
fun map (f, xs) =
    case xs of
	[] => []
     | 	x :: [] => [f(x)]
     |  x::xl' => f x :: map(f,xl')


fun multiply_2 x = x * 2

			   
val x = map(multiply_2,[2,4,6,7,8,9]);
val x2 = map(hd,[[1,2],[3,4],[5,6]]);

fun filter(f,xs) =
    case xs of
	[] => []
      | x :: xl => if f x then x :: filter(f,xl)
		   else
		       filter(f,xl)
			     
val y = filter((fn x => x < 4), [1,2,3,4,5,0]);


fun is_even v =
    (v mod 2 = 0)
fun all_even xs = filter(is_even, xs);
fun all_even_snd xs = filter((fn(_,v) => is_even v), xs);


			    
