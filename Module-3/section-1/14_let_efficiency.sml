(* Programming Languages, Dan Grossman *)
(* Section 1: Let Expressions to Avoid Repeated Computation *)

(* badly named(: evaluates to 0 on empty list *)
fun bad_max (xs : int list) =
    if null xs
    then 0
    else if null (tl xs)
    then hd xs
    else if hd xs > bad_max(tl xs)
    then hd xs
    else bad_max(tl xs)

fun good_max (xs : int list, n: int) =
    if null xs
    then n
    else if hd xs > n
    then good_max(tl xs, hd xs)
    else good_max(tl xs, n)

fun good_max_v2 (xs : int list) =
    if null xs
    then 0
    else if null (tl xs)
    then hd xs
    else
	let val tl_ans = good_max_v2 (tl xs)
	in
	    if hd xs > tl_ans
	    then hd xs
	    else tl_ans
	end
	    
		 
fun countup_from1 (x : int) =
    let
    fun count (from : int) =
    if from = x
    then x::[]
    else from :: count(from + 1)
    in
	count(1)
    end


fun countdown (x : int) =
    if x = 0
    then []
    else  x :: countdown(x-1)
  
