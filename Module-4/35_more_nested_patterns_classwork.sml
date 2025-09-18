(* Section 2: More Nested Patterns *)

fun nondecreasing xs = (* int list -> bool *)
    case xs of
	[] => true
      | _::[] => true
      | head::(neck::rest) => head <= neck
			      andalso nondecreasing rest


datatype sgn = P | N | Z
			   
fun multsign (x1, x2) = (* int * int -> sgn *)
    let fun sign x = if X = 0 then Z else if X > 0  then P else N
    in
	case (sign x1, sign x2)  of
	    (Z,_) => Z
	  | (_,Z) => Z
	  | (P,P) => P
	  | (N,N) => P		 
	  | (N,P) => N 
	  | (P,N) => N
	  
    end
	
			    
				      
fun len xs =
    case xs of
	[] => 0
      | _::xs' => 1 + len xs'
			 
								 
    
								      
