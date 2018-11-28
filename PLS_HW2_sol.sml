(* Preparation task: changing bool func *)
fun negate f r = 
    if f(r) = true
    then false
    else true;

negate (fn () => false) ();
negate (fn () => true) ();
negate (fn () => false);
negate (fn a => a) false;


(* Assignment 1-A *)
fun assertThat value matcher input 
    = (matcher(input)value):bool;
    
assertThat 5 (fn a => fn b => a=b) 5;
assertThat 5 (fn a => fn b => a=b) 6;
assertThat 5 (fn a => fn b => a<b) 5;

(* Assignment 1-B *)
fun equalTo a b = a=b;

assertThat 5 equalTo 5;
assertThat 5 equalTo 6;

(* Assignment 1-C *)
fun doesnt matcher a =
    if matcher(a) = true
    then false
    else true;

assertThat 5 doesnt (equalTo 5);
assertThat 5 doesnt (equalTo 6);

(* Assignment 1-D *)
fun bothOf (f1,f2) a =
    if f1(a) = true = f2(a)
    then true
    else false;

assertThat 5 bothOf (doesnt (equalTo 6), equalTo 5);
assertThat 5 bothOf (doesnt (equalTo 6), equalTo 6);
assertThat 5 bothOf (doesnt (equalTo 5), equalTo 5);


(* Assignment 2: Sum the letter's counter *)
local
    fun char_to_int c =
        if ord c > 90
			then ord c - 96
			else ord c - 64;
    fun string_i_to_int s i = char_to_int (String.sub(s,i));
    
    infix @; (* list combiner *)
    fun [] @ ys = ys
        | (x::xs) @ ys = x::(xs@ys);
    
    fun prefix_sum_list s i sum =
        if size s <= i
        then []
        else [string_i_to_int s i + sum] @
            prefix_sum_list s (i + 1) (string_i_to_int s i + sum);
in
    fun prefixSum s = prefix_sum_list s 0 0
end;

prefixSum "zA";
prefixSum "";
prefixSum "tomer";
prefixSum "Tomer";
prefixSum "tOmEr";

(* Assignment 3: Make specific signatures *)
fun sig1 x y f = if 1>0 then y else f(x,y);
fun sig2 (i,r) f = if f(r*1.5)="h" andalso i+1>1 then true else false; 
fun sig3 f a b d = ((f a) b); 
fun sig4 a b c d =  c+d;
fun sig5 f a g = g(f(a),f(a));
fun sig6 () () = ();
fun sig7 f a b = if 1>0 then f(tl a) else f(b);

 
