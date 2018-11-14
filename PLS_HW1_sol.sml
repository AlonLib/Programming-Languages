(* Question 1: Balanced brackets *)
fun brackets(s,i,j) = 
    if (size s)=i
    then
        if j=0
        then true
        else false
    else 
        if String.sub(s,i) = #"("
        then brackets(s,i+1,j+1)
        else
            if String.sub(s,i) = #")"
            then
                if j=0
                then false
                else brackets(s,i+1,j-1)
            else brackets(s,i+1,j);

fun balance s = brackets(s,0,0);


(* Question 2: String to int *)
fun ten n = 
    if n<=1
    then 1
    else 10 * ten(n-1);

fun numbers(s,i) = 
    if (size s)=i
    then
        0
    else 
         (ord (String.sub(s,i)) - 48) * (ten ((size s) - i)) + numbers(s,i+1);;

fun atoi s = numbers(s,0);


(* Question 3: Reverse string *)
fun backwards(s,n) = 
    if n=0
    then ""
    else str (String.sub(s,n-1)) ^ backwards(s,n-1);

fun reverseString s = backwards(s,(size s));


(* Question 4: Extended Euclidean algorithm *)
fun helper ((d1:int,s1:int,t1:int),m:int,n:int) =
    if d1>=0 then (d1,t1,(s1-(m div n)*t1))
    else (~d1,~t1,~(s1-(m div n)*t1));

fun extended (m,n) = 
    if n=0 then (m,1,0)
else helper(extended(n,m mod n),m,n);
