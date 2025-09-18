(* Elaine Tomczyk *)
(* Programming Languages *)
(* Homework 1 *)
(* Functions related to calendar dates.  
   Reasonable Calendar date is defined as a 3-tuple of int (int*int*int) where 
   #1 is the year and greater than 0
   #2 is the month and between 1 and 12
   #3 is the day and not greater than 31
 These functions will work for reasonable calendar dates, but do not check for 
 them.
  A day of year is an int from 1 to 365.  *)


(*********************************************************************)
(*  Function 1 
    is_older:  ((int*int*int)*(int*int*int))->bool
    accepts two reasonable dates
    returns true if first date occurs before second date
     otherwise false *)

fun is_older(date1: int*int*int, date2: int*int*int) =
  let (*assign a day value to a date assuming 365 days/year and 30 days/month *)
      fun day_val(date:int*int*int)=
	((#1 date) * 365) + ((#2 date)* 30) + #3 date
  in
      if (day_val(date1))<(day_val(date2))
			   then true
		           else false
  end


(*********************************************************************)
(*  Function 2
    number_in_month: ((int*int*int)list)*int -> int
      accepts a list of reasonable dates and an integer month
      returns a count of the number of dates in the list with
                      the given month *)
fun number_in_month(dates:(int*int*int) list, month:int)=
  if null dates
      then 0 (* no dates to check *)
  else
      if (  (#2 (hd dates)  ) = month)
          then 1+ number_in_month(tl dates, month)
      else
	  number_in_month(tl dates, month)


(*********************************************************************)
(*  Function 3
    number_in_months: ((int*int*int)list) * int list -> int
    accepts a list of reasonable dates and a list of months
    returns a count of the number of dates that occur in 
    any of the months in the list
    *** assume no repeats in the month list *)
fun number_in_months(dates:(int*int*int) list, months:int list)=
  if null months
  then 0 (*no months to check*)
  else
      number_in_month(dates,(hd months))+ number_in_months(dates,(tl months))


(*********************************************************************)
(*  Function 4
    dates_in_month: ((int*int*int)list * int -> (int*int*int) list
    accepts a list of reasonable dates and an integer month
    returns a list of those dates that occur in the given month*)
fun dates_in_month (dates:(int*int*int) list, month:int) =
  if null dates
  then [] (*no dates to check*)
  else
      if ( (#2 (hd dates)) ) = month
	   then (hd dates) :: (dates_in_month( (tl dates),month))
           else dates_in_month((tl dates),month)
						  

(*********************************************************************)
(*  Function 5
   dates_in_months : ((int*int*int) list * int list -> (int*int*int) list
   accepts a list of reasonable dates and a list of integer months
   returns a list of those dates occuring in given months*)
fun dates_in_months(dates:(int*int*int) list, months: int list) =
  if null months
  then [] (*no months to check*)
  else
      dates_in_month(dates,(hd months)) @ dates_in_months(dates,(tl months))


(*********************************************************************)
(*  Funtion 6
    get_nth : (str list* int -> str
    accepts a list of strings and an integer, n
    returns the nth string in the list
 *)
fun get_nth( ss:string list, n:int)=
  if null ss
  then "ran out of strings to check"
  else
      if n=1 then
	  hd ss (* nth string is at the head of the list *)
      else get_nth(tl ss, n-1) (*count the first element and remove it*)


(*********************************************************************)
(*   Function 7
     date_to_string: (int*int*int)-> string
     accepts a reasonable date and
     returns the date as a string in the format "month day, year" *)
fun date_to_string(date:(int*int*int))=
  let
      val months_list=["January","February","March","April","May","June","July","August","September","October","November","December"]
      val month = get_nth(months_list,#2 date)
      val day = Int.toString(#3 date)
      val year = Int.toString(#1 date)
  in
      month^" "^day^", "^year
  end


(*********************************************************************)
 (*  Function 8 
     number_before_reaching_sum: int * int list -> int
     accepts a positive integer(sum) and a list of positive integers 
     returns an integer n such that the first n elements of the list add up to but do not 
       exceed the sum, and adding the first n+1 elements will equal or exceed the given sum. *)

fun number_before_reaching_sum(sum:int,pos_ints:int list)=


  (*sum_up : int * int list * int -> int  
    **This function sums up the integers in a list until the limit is reached or exceeded.
    accepts  the current sum, list remaining to be summed up and a count of elements in sum  
    returns the last count of elements before current sum exceeded given sum *)
  let fun sum_up (curr_sum:int,curr_list:int list,n:int)=
 
	if curr_sum >= sum  (*check current sum against sum passed in *)
	then n-1 (*went over the limit, so go back one and return your count *)
	else
	 sum_up((curr_sum+hd curr_list),tl curr_list,n+1)(*not there yet, add another element to the sum*)  
  in
      sum_up(hd pos_ints,tl pos_ints,1) (*start with the current sum the first list element, the rest of the list to be added and a count of 1-the first element*)
  end


(*********************************************************************)
(*   Function 9
     what_month: int -> int
     accepts a day of the year (int between 1 and 365) and 
     returns the month (1 through 12) which that day is in.
     
     uses function number_before_reaching_sum to help determine month
     since adding a month at a time returns the last full month, need to add one for the 
     partial month  *)


fun what_month(day:int) =

  let val days_in_months = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
     1+ number_before_reaching_sum(day,days_in_months)
  end


(*********************************************************************)
(*  Function 10
    month_range: int * int -> int list
    accepts two days of the year (int between 1 and 365) 
    returns a list of months for each day between day1 and day2 inclusive.
        *that is: the list begins with the month of day1, followed by the month of day1+1, continuing until it ends with the month of day2
    uses functions
    
    what_month to determine month associated with each day
    
 *)
fun month_range (day1:int, day2:int) =
  let fun day_countup (from:int, to:int)=
 	      if from = to
	      then what_month(to)::[]
	      else what_month(from) :: day_countup(from+1,to)
	    
  in
      if day1 > day2
      then []
      else day_countup (day1,day2)
  end

(**********************************************************************)
(*   Function 11
     oldest: (int*int*int)list-> (int*int*int)option
     accepts a list of reasonable dates
     returns NONE if the list has no dates and SOME d where d is the oldest date in the list
 *)
fun oldest (date_list:(int*int*int) list) =
  if null date_list
  then NONE
  else
      let fun find_oldest(old:(int*int*int),dates:(int*int*int) list) =
	     if null dates
	    then old (*no more to compare, so oldest is found*)
	    else
		if is_older(hd dates, old) (*next date in list older than oldest thus far*)
		then find_oldest(hd dates,tl dates)
		else find_oldest(old, tl dates)
      in
	  SOME (find_oldest(hd date_list, tl date_list))
      end
	  
				