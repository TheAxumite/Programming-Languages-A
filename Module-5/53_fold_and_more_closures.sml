(* Programming Languages, Dan Grossman *)
(* Section 3: Fold and More Closures *)

(* Another hall-of-fame higher-order function *)

(* note this is "fold left" if order matters 
   can also do "fold right" *)
fun fold (f,acc,xs) =
    case xs of 
	[] => acc
      | x::xs' => fold (f,f(acc,x),xs')

(* examples not using private data *)

fun f1 xs = fold ((fn (x,y) => x+y), 0, xs)(* Sums the list *)

fun f2 xs = fold ((fn (x,y) => x andalso y >= 0), true, xs) (* Returns true if all the numbers in the list are greater than -1 *)

(* examples using private data *)
(* Counting the number of elements between low and hi *)
fun f3 (xs,lo,hi) = 
    fold ((fn (x,y) => 
	      x + (if y >= lo andalso y <= hi then 1 else 0)),
          0, xs)
(* checks to see if the size of strings in the list are less than s *)
fun f4 (xs,s) =
    let 
	val i = String.size s
    in
	fold((fn (x,y) => x andalso String.size y < i), true, xs)
    end

fun f5 (g,xs) = fold((fn(x,y) => x andalso g y), true, xs)

fun f4again (xs,s) =
    let
	val i = String.size s
    in
	f5(fn y => String.size y < i, xs)
    end


val test_f5 = f5 ((fn x => x + 3 < 9), [0,1,2,3,4]);
