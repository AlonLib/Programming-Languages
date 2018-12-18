

(* Exercise 1: Do not use if, pattern matching, case, recursive, let/local *)

(* Ex A: return the n element from a given list *)
infix at
fun a at n = hd (List.drop (a,n));

[1, 2, 3, 4] at 3;
[true, false, true] at 0;


(* Ex B: Convert a list [a,b,c] to [(0,a),(1,b),(2,c)] *)
fun enumerate a = rev (foldr (fn (a,b) => (length b,a)::b) [] (rev a));

enumerate ["A", "B", "C", "D"];
enumerate [];

(* Ex C: Reverse a given list *)
fun reverse a = rev a;

reverse [false, true, true, false ,false];
rev ["A", "B", "C", "D"];
rev [];

(* Ex D: Convert list of lists to one list *)
fun flatten a = foldr op@ [] a;

flatten [[1, 2, 3], [4, 5, 6], [7, 8]];
flatten [];
flatten [[], [1]];

(* Ex E: Converting a list using applyif *)
fun applyif f pred a = map (fn x => hd ((map f (List.filter pred [x]))@[x])) a;

applyif (fn x => x * x) (fn x => x > 5) [1, 3, 5, 7, 9, 11];
applyif (fn x => (fn x => x-10)) (fn x => x(7) < 10) [fn x => x+8, fn x => x * x, fn x => 12, fn x => x mod 13];

(* Ex F: Retunrs a new list from index e to index (e-1). *)
fun slice a (s,e) = List.drop ((List.take (a,e)),s);

slice [0, 0, 1, 1] (2, 4);
slice [0, 0, 1, 0] (2, 3);
slice [1, 1, 0, 0] (0, 2);
slice [0, 0, 0, 0] (1, 1);
slice [] (0, 0);

(* Ex G: Returns a new list where all elements satisfies the predicates pred *)
fun allholds pred a = foldr (fn (pred,a) => (List.filter pred a)) a pred;

allholds [] [1, 2, 3];
allholds [(fn x => true)] [1, 2, 3];
allholds [(fn x => false)] [1, 2, 3];
allholds [fn x => x mod 2 = 0, fn y => y mod 3 = 0] [1, 3, 6, 8, 12, 18, 9];



