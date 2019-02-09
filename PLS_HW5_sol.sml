(*--------------------------------------------------------------------------------------------------*)
(*------------------------------------------- Question 2 -------------------------------------------*)
(*--------------------------------------------------------------------------------------------------*)

datatype 'a seq = Nil | Cons of 'a * (unit-> 'a seq);

exception EmptySeq;
fun head(Cons(x,_)) = x | head Nil = raise EmptySeq;
fun tail(Cons(_,xf)) = xf() | tail Nil = raise EmptySeq;

datatype direction = Back | Forward;
datatype 'a bseq =   bNil
                | bCons of 'a * (direction -> 'a bseq);

fun bHead(bCons(x,_)) = x | bHead bNil = raise EmptySeq;
fun bForward(bCons(_,xf)) = xf(Forward) | bForward bNil = raise EmptySeq;
fun bBack(bCons(_,xf)) = xf(Back) | bBack bNil = raise EmptySeq;

(*---------------- Question 2 Section 1 ----------------*)
fun intbseq x = 
	bCons(x,fn Forward => intbseq (x+1) | Back => intbseq (x-1));

(*---------------- Question 2 Section 2 ----------------*)
fun bmap f (bCons (x,xf)) = 
	bCons(f x,
		fn Forward => bmap f (xf Forward)
			| Back => bmap f (xf Back))
	| bmap _ _ = bNil;

(*---------------- Question 2 Section 3 ----------------*)
fun bfilter pred d (bCons (x,xf)) = 
	if (pred x = true)
	then bCons(x,
			fn Forward => bfilter pred Forward (xf Forward)
				| Back => bfilter pred Back (xf Back))
	else if (d = Forward)
		then bfilter pred d (xf Forward)
		else bfilter pred d (xf Back)
	| bfilter _ _ _ = bNil;

(*---------------- Question 2 Section 4 ----------------*)
(* there is a better solution *)
local
	exception Zero_Target;

	fun get_n_obj s 1 = head s
		| get_n_obj _ 0 = raise Zero_Target
		| get_n_obj s n = get_n_obj (tail s) (n-1);

	fun helper a b (~1) Forward = bCons (head b,(helper a b 1))
		| helper a b 1 Back = bCons (head a,(helper a b (~1)))
		| helper _ _ 0 _ = raise Zero_Target
		| helper a b n Forward = 
			if n>0
				then bCons (get_n_obj b (n+1),helper a b (n+1))
				else bCons (get_n_obj a (abs (n+1)),helper a b (n+1))
		| helper a b n Back =
			if n>0
				then bCons (get_n_obj b (n-1),helper a b (n-1))
				else bCons (get_n_obj a (abs (n-1)),helper a b (n-1));
in
	fun seq2bseq a b = helper a b 2 Back;
end;

(*---------------- Question 2 Section 5 ----------------*)
local
	fun jump_n 0 s _ = s
		| jump_n n (bCons (x,xf)) d = jump_n (n-1) (xf d) d
		| jump_n _ _ _ = raise Match;
in
	fun bSeqJump s n = 
	bCons(bHead s,fn d => bSeqJump (jump_n n s d) n);
end;

