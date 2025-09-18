(* Programming Languages, Dan Grossman *)
(* Section 2: Tail Recursion *)

fun fact1 n = if n=0 then 1 else n * fact1(n-1)

fun fact2 n =
    let fun aux(n,acc) = if n=0 then acc else aux(n-1,acc*n)
    in
        aux(n,1)
    end

datatype person = Name of string * (string option) * string
		| Age of int
		| Birthdate of int * int * int

val person1 = (Name("leul", NONE, "Dessalegn"), Age(40), Birthdate(04,17,1985));

val person2 = (Name("real", NONE, "Dijon"), Age(4), Birthdate(04,17,1995));
    
val lop = [person1, person2];		  

fun retrieve_name lon =
     let fun retrieve (lop, n_list) =
	     case lop  of
		[] => n_list
	       |(Name(fi,mi,la),_,_) :: tl => retrieve(tl, fi :: n_list)
	       | (_,_,Name(fi,mi,la)) :: tl => retrieve(tl, fi :: n_list)
	       | (_,Name(fi,mi,la),_) :: tl => retrieve(tl, fi :: n_list)
	       | (_,_,_) :: tl => n_list 
		  in
		      retrieve(lon, [])
			      		
    end

	
val first_name = retrieve_name lop;
