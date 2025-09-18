(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

			       
(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

	    

	   
				    
fun only_capitals xs = List.filter (fn x => Char.isUpper(String.sub(x,0))) xs
				
fun longest_string1 xs = foldl (fn (x,y) => if String.size(x) > String.size(y) then x else y) "" xs

fun longest_string2 xs = foldl (fn (x,y) => if String.size(x) >= String.size(y) then x else y) "" xs
			       					    
fun longest_string_helper cmp xs = List.foldl (fn (x, acc) => if cmp (String.size x, String.size acc) then x else acc) "" xs;

val longest_string3 = longest_string_helper (fn(x,y) => x > y)

val longest_string4 = longest_string_helper (fn(x,y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals
				
val rev_string =  String.implode o List.rev o String.explode					      

fun first_answer opt xs = 
    let
	val test = List.find(fn x => opt x = SOME x) xs in
    case test of
	NONE  => raise NoAnswer
      | SOME x => x
    end

fun helper f xs acc =
    case xs of
	[] => SOME acc
      | x::xl  =>  case f(x) of
		       NONE => NONE
		     | SOME y => helper f xl (y @ acc)
fun all_answers f xs = helper f xs []

fun count_wildcards p =  g(fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p = g (fn () => 1) (fn x => String.size x) p

(* fun helper patternstring = (fn => f1) => List.foldl(fn (patternletter, acc) => List.length(List.filter(fn stringletter => stringletter = patternletter) f1) + acc) 0 String.explode patternstring *)
												 
		  
fun count_some_var (s, pat)  = g (fn () => 0) (fn x => if s = x then 1 else 0) pat
				 
(* fun stringtolist p = case p of Variable x => List.map(fn y => Char.toString y) (String.explode x)*)

fun list_of_patterns pl =
    let fun check_pattern p acc =
	    case p of
		Wildcard          => acc
	      | Variable x        => x::acc
	      | TupleP ps         => List.foldl (fn (x,acc) => check_pattern x acc) acc ps 
	      | ConstructorP(s,p) => check_pattern p acc
	      | _                 => acc
    in
	check_pattern pl []
    end
	
fun repeats xs = List.foldl(fn (x,acc) => if List.length(List.filter(fn y => x = y) xs) > 1 then false else acc) true xs
val combinedhelper = repeats o list_of_patterns
fun check_pat xs = combinedhelper xs				   

				   
fun match (v,p) =
    case (v, p) of
	(_,Wildcard) => SOME []
      | (_, Variable x) => SOME [(x,v)]
      | (Unit,UnitP) => SOME []
      | (Const x, ConstP y) => if x = y then SOME [] else NONE  
      | (Tuple vl, TupleP ps) => if List.length(vl) = List.length(ps) then all_answers (fn (vl,ps) => match(vl,ps)) (ListPair.zip(vl,ps)) else NONE				
      | (Constructor(s1,vu), ConstructorP(s2,pa)) => if s1 = s2  then match(vu,pa) else NONE
      | (_,_) => NONE
		      		   
		  
fun first_match v pl = first_answer (fn x => case x of SOME y => SOME x | _ => NONE)(List.map (fn x => match(v, x)) pl) handle NoAnswer => NONE


fun longest_string2 xs = foldl (fn (x,y) => if String.size(x) >= String.size(y) then x else y) "" xs

fun longest_string_helper cmp xs = List.foldl (fn (x, acc) => if cmp (String.size x, String.size acc) then x else acc) "" xs;
									 
val longest_string3 = longest_string_helper (fn(x,y) => x > y)

val longest_string4 = longest_string_helper (fn(x,y) => x >= y)
