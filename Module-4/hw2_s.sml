(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(s, slist) =
  let
    fun remove_from_list(l, acc) =
      case l of
        [] => ([], acc)
      | (head::rest) => if same_string(head, s)
                        then remove_from_list(rest, acc)
                        else remove_from_list(rest, acc @ [head])

    fun check(state) =
      case state of
        ([], _) => ([], false)
      | (head::rest, _) => if same_string(head, s)
                           then 
                             let val (_, acc) = remove_from_list(slist, []);
                             in (acc, true)
                             end
                           else (check(rest, false))
  in
    case check(slist, false) of
      (_, false) => NONE
    | (acc, true) => SOME acc
  end

fun get_substitutions1(slistlist, s) =
  case slistlist of
    [] => []
  | head::rest => case all_except_option(s, head) of
                    NONE => get_substitutions1(rest, s)
                  | SOME found => found @ get_substitutions1(rest, s)

fun get_substitutions2(slistlist, s) =
  let fun helper(state) =
    case state of
      ([], acc) => ([], acc)
    | (head::rest, acc) => case all_except_option(s, head) of
                             NONE => helper(rest, acc)
                           | SOME found => helper(rest, acc @ found)
  in
    case helper(slistlist, []) of
      (_, acc) => acc
  end

fun similar_names(slistlist, {first=first, last=last, middle=middle}) =
  let
    fun to_full_names(names) =
      case names of
        [] => []
      | head::rest => {first=head, middle=middle, last=last} :: to_full_names(rest)

    fun helper(state) =
      case state of
        ([], acc) => ([], acc)
      | (head::rest, acc) =>
          case all_except_option(first, head) of
            NONE => helper(rest, acc)
          | SOME found => helper(rest, acc @ to_full_names(found))
  in
    case helper(slistlist, [{first=first, last=last, middle=middle}]) of
      (_, acc) => acc
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color(c) =
  case c of
    (Clubs, _) => Black
  | (Diamonds, _) => Red
  | (Hearts, _) => Red
  | (Spades, _) => Black

fun card_value(c) =
  case c of
    (_, Jack) => 10
  | (_, Queen) => 10
  | (_, King) => 10
  | (_, Ace) => 11
  | (_, Num num) => num


fun remove_card(cs, c, exc) =
  let
    fun remove_first_occurence(l, acc) =
      case l of
        [] => ([], acc)
      | head::rest => if head = c
                        then ([], acc @ rest)
                        else remove_first_occurence(rest, acc @ [head])

  in
    case cs of
      [] => raise exc
    | oldcs => let val (_, newcs) = remove_first_occurence(oldcs, [])
           in if newcs = oldcs
              then raise exc
              else newcs
           end
  end

fun all_same_color(cs) =
  let
    fun same_color(cs, color) =
      case cs of
        [] => true
      | head::rest => if card_color(head) = color
                      then same_color(rest, color)
                      else false
    val head::_ = cs
  in
    same_color(cs, card_color(head))
  end

fun sum_cards(cs) =
  let 
    fun sum(cs, acc) =
      case cs of
        [] => acc
      | head::rest => sum(rest, acc + card_value(head))
  in
    sum(cs, 0)
  end

fun score(cl, goal) =
  let
    val cl_sum = sum_cards(cl)
    val preliminary_score = if cl_sum > goal
                            then 3 * (cl_sum - goal)
                            else goal - cl_sum
  in
    if all_same_color(cl) = true
    then preliminary_score div 2
    else preliminary_score
  end

exception EndOfGame of card list

fun officiate(cl, ml, goal) =
  let
    fun game(cl, ml, hc) =
      case ml of
        [] => raise EndOfGame(hc)
      | move::rest => case move of
                        Discard c => game(cl, rest, remove_card(hc, c, IllegalMove))
                      | Draw => case cl of
                                  [] => raise EndOfGame(hc)
                                | hcl::clrest => 
                                    let
                                      val new_hc = hcl :: hc
                                      val hc_sum = sum_cards(new_hc)
                                    in
                                      if hc_sum > goal
                                      then raise EndOfGame(new_hc)
                                      else game(clrest, rest, new_hc)
                                    end
  in
    game(cl, ml, [])
    handle EndOfGame x => score(x, goal)
  end
