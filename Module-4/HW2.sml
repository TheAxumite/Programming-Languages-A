(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (s1 : string, lst : string list) =
    case lst of
	[] => NONE
      | x::xl => if same_string(s1,x) then
		     SOME xl
		 else
		     case all_except_option(s1, xl) of
			 NONE => NONE
		       | SOME li => SOME(x :: li)
				       
fun get_substitutions1 (s_list, s)  = 
    case s_list of
	[] => []
      | x::xl => case all_except_option (s,x) of
		     NONE => get_substitutions1 (xl,s)
		   | SOME li  => li @ get_substitutions1 (xl,s)
							 
fun get_substitutions2 (s_list, s) =
    let fun helper (li, acc) =
	    case li of
		[] => acc
	      | x::xl => case all_except_option (s, x) of
			     NONE => helper (xl, acc)
			   | SOME si  => helper (xl, acc @ si)			  
    in
	helper(s_list,[])
    end
	

fun similar_names (s_list, {first, middle, last}) = (* Using pattern matching for clarity *)
    let fun helper(sub, acc) =
        case sub of
            [] => List.rev acc
          | x :: xl  => helper(xl, {first = x, middle = middle, last = last} :: acc)
    in helper(get_substitutions2(s_list, first), [{first = first, middle = middle, last = last}])
    end	
		       
    
									
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove
val c1 : card = (Clubs, Num 2);
val c2 : card = (Diamonds,Ace);

fun card_color c =
    case c of	
      (Clubs,_)  => Black
     | (Spades,_) => Black
     | (Diamonds , _) => Red
     | (Hearts,_) => Red 
		      
fun card_value c =
    case c of
	(_,Num x)  => x
      | (_,Ace) => 11
      | (_,_) => 10
			
     		      
fun remove_card (cs : card list, c, e) =
    case cs of
	[] => raise e
      | x::xy => if c = x then
		     xy
		 else
		    x :: remove_card(xy, c, e) 
			 
fun all_same_color (lc : card list) =
    case lc of
	[] => true
      | x::[] => true
      | x::(x1::xy) => card_color x = card_color x1 andalso all_same_color(x1::xy)


fun sum_cards (lc : card list) =
    let fun add (c, acc) =
	    case c of
		[] => acc
	      | x :: x1 => add (x1, card_value x + acc)
    in
	add(lc,0)
    end
	
fun score (lc, go) =
    let fun color sum = if all_same_color lc then sum div 2 else sum
								   
    in
	
    case sum_cards lc of
	x => if x > go then color (3 * (x - go))
	     else
		 color (go - x)
		     
    end
	

fun officiate (cl,m,g) =
    let fun track_state (hlc,cl,moves) =
	    let fun adjust_state (heldcards,cardlist,move,moves) =
		    case move of
			Discard card => track_state(remove_card (heldcards, card, IllegalMove),cardlist,moves)
		     |  Draw  => case cardlist of
				     [] => track_state(heldcards,cardlist,moves)
				   | x :: xy => track_state(x::heldcards,remove_card(cardlist,x,IllegalMove),moves)
	    in
		if sum_cards hlc <= g then
		    case moves of
			[] => hlc
		      | x :: xy  => adjust_state (hlc,cl,x,xy)
		else
		    hlc
	    end		
    in
	score(track_state([],cl,m),g)
    end
	    
									
								   
					     
							       
				 
				       
						 



						     
						     
