(* Programming Languages, Dan Grossman *)
(* Section 2: Exceptions *)

val x = List.Empty;

val res = (hd [], 0) handle List.Empty => raise x;

															      
fun hd xs =
    case xs of
	[] => raise List.Empty
      | x::_  => x
		     
exception MyOtherException of int * int

fun multiplytuple (xs) =
    case xs of
	(0,0) => raise MyOtherException xs
      | (x,y)  => x * y
			  
val test = multiplytuple(2,3);

val test2 = multiplytuple(0,0) handle MyOtherException(_,_) => 32;

exception MyUndesirableCondition
					
fun mydiv (x,y) =
    if y=0
    then raise MyUndesirableCondition
    else x div y
		   
fun maxlist (xs,ex) =
    case xs of
	[] => raise ex
     |  x::[] => x
     | x::xs' => Int.max(x,maxlist(xs',ex))

val i = mydiv (3,0);
val w = maxlist ([3,4,5], MyUndesirableCondition)


val x = maxlist ([3,4,5], MyUndesirableCondition)
	handle MyUndesirableCondition => 42

(*val y = maxlist ([], MyUndesirableCondition) *)

val z = maxlist ([], MyUndesirableCondition)
		
					     

