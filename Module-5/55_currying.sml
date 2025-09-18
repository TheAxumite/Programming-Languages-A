(* Programming Languages, Dan Grossman *)
(* Section 3: Another Closure Idiom: Currying *)

(* old way to get the effect of multiple arguments *)
fun sorted3_tupled (x,y,z) = z >= y andalso y >= x

val t1 = sorted3_tupled (7,9,11)

(* new way: currying *)

val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x

(* fun sorted3 x = fn y => fn z => ... *)

val t2 = ((sorted3 7) 9) 11
		 
val t3 = sorted3 7 9 3	   
							      


fun sorted3_nicer x y z = z >= y andalso y >= x

val t4 = sorted3_nicer 7 9 11
		       
		       (* a more usful example *)
fun fold f acc xs = (* means fun fold f = fn acc => fn xs => *)
    case xs of
	[] => acc
      | x::xs' => fold f (f(acc,x)) xs'
		       
fun sum xs = fold (fn (x,y)=> x + y) 0 xs
		  
val t5 = sum [4,3,2,1]
