(* datas has type (int * int * int) *)
(* Question 01 *)
fun is_older(t1 : (int * int * int), t2 : (int * int * int)) = 
    if (#1 t2) > (#1 t1)
    then true
    else 
        if (#1 t2) = (#1 t1)
        then 
            if (#2 t2) > (#2 t1)
            then true
            else
                if (#2 t2) = (#2 t1)
                then
                    if (#3 t2) > (#3 t1) 
                    then true 
                    else false
                else false
        else false
                    
(* Question 02 *)
fun number_in_month(dates : (int*int*int) list, nth_month) = 
    if null dates
    then 0
    else
        if (#2 (hd dates)) = nth_month
        then 1 + number_in_month(tl dates, nth_month)
        else number_in_month(tl dates, nth_month)

(* Question 03 *)
fun number_in_months(dates : (int * int * int) list, nth_months : int list) =
    if null dates
    then 0
    else
        if null nth_months
        then 0 
        else number_in_month(dates, hd nth_months) + number_in_months(dates, tl nth_months)

(* Question 04 *)
fun dates_in_month(dates : (int*int*int) list, nth_month) = 
    if null dates
    then []
    else
        if (#2 (hd dates)) = nth_month
        then (hd dates)::dates_in_month(tl dates, nth_month)
        else dates_in_month(tl dates, nth_month)

(* Question 05 *)
fun dates_in_months(dates : (int * int * int) list, nth_months : int list) =
    if null dates
    then []
    else
        if null nth_months
        then []
        else dates_in_month(dates, hd nth_months) @ dates_in_months(dates, tl nth_months)

(* Question 06 *)
fun get_nth(lst: string list, nth : int) = 
    if nth = 1
    then hd lst
    else get_nth(tl lst, nth - 1)


(* Question 07 *)
fun date_to_string(date : (int * int * int)) = 
    let
        val months = ["January", "February", "March", "April", "May", "June", 
            "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

(* Question 08 *)
fun number_before_reaching_sum(num : int, lst: int list) = 
    if (num - (hd lst))<= 0
    then 0
    else 1 + number_before_reaching_sum(num - (hd lst), tl lst)

(* Question 09 *)
fun what_month (nth_day : int) = 
    let
        val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(nth_day, months) + 1
    end

(* Question 10 *)
fun month_range(day1, day2) = 
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1 + 1, day2)

(* Question 11 *)
fun oldest(dates : (int * int * int) list) = 
    if null dates
    then NONE
    else
        let
            val tl_oldest = oldest(tl dates)
        in
            if isSome tl_oldest andalso not (is_older(hd dates, valOf tl_oldest))
            then tl_oldest
            else SOME (hd dates)
        end