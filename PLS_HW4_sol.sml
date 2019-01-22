

(* Section 1+6+7 *)
datatype S =
    INT of int
    | STR of string
    | NIL
    | CONS of S*S (* CAR is left and CDR is right *)
    | NORMAL of FUNCTION (* SETQ,COND,QUOTE *)
    | EAGER of FUNCTION
and FUNCTION = 
	UNARY of S -> S -> S
	| BINARY of S * S -> S -> S
	| TRINARY of S * S * S -> S -> S;

(* Section 2 *)
val T = STR "T";

local
	(* ADDONS TO HIDE LATER *)
	infix ==
	fun op== (INT a,INT b) = a=b
		| op== (STR a,STR b) = a=b
		| op== (CONS a,CONS b) = (#1 a) == (#1 b) andalso (#2 a) == (#2 b)
		| op== (NIL,NIL) = true
		| op== _ = false;

in

	(* Section 3 *)
	fun CAR (CONS (car,cdr)) (_:S) = car
		| CAR _ _ = raise Match;
	fun CDR (CONS (car,cdr)) (_:S) = cdr
		| CDR _ _ = raise Match;
	fun NULL NIL (_:S) = T
		| NULL _ _ = NIL;
	fun INTEGER (INT s) (_:S) = T
		| INTEGER _ _ = NIL;
	fun QUOTE (x:S) (_:S) = x; (* this is a NORMAL function *)
	fun LST NIL (_:S) = T
		| LST (CONS (CONS s,cdr)) env = 
			if (LST cdr env == T andalso LST (CONS s) env == T)
				then T
			else NIL
		| LST (CONS (car,cdr)) env = 
			if (LST cdr env == T)
				then T
			else NIL
		| LST _ _ = NIL;

	(* Section 4 *)
	fun EQ (CONS (car1,cdr1),CONS (car2,cdr2)) env = 
			if (EQ (car1,car2) env == T andalso EQ (cdr1,cdr2) env == T)
				then T
			else NIL
		| EQ (s1,s2) (_:S) =
			if (s1 == s2)
				then T
			else NIL;
	fun PLUS (INT a,INT b) (_:S) = INT (a+b)
		| PLUS _ _ = raise Match;
	fun TIMES (INT a,INT b) (_:S) = INT (a*b)
		| TIMES _ _ = raise Match;
	fun MEANING (STR IDENTIFIER,CONS (CONS (STR caar,cadr),cdr)) env =
		if (IDENTIFIER = caar)
			then cadr
			else MEANING(STR IDENTIFIER,cdr) env
		| MEANING (_,NIL) (_:S) = NIL
		| MEANING _ _ = raise Match;

	(* Section 8+9 *)
	fun EVAL (NIL,_) = NIL
		| EVAL (INT num,_) = INT num
		| EVAL (STR str,env) = MEANING (STR str,env) env
		| EVAL (CONS (NORMAL (UNARY func),CONS (arg,NIL)),env) =
			func arg env
		| EVAL (CONS (NORMAL (BINARY func),CONS (arg1,CONS (arg2,NIL))),env) =
			func (arg1,arg2) env
		| EVAL (CONS (NORMAL (TRINARY func),CONS (arg1,CONS (arg2,CONS (arg3,NIL)))),env) =
			func (arg1,arg2,arg3) env
		| EVAL (CONS (EAGER (UNARY func),CONS (arg,NIL)),env) =
			func (EVAL (arg,env)) env
		| EVAL (CONS (EAGER (BINARY func),CONS (arg1,CONS (arg2,NIL))),env) =
			func (EVAL (arg1,env),EVAL (arg2,env)) env
		| EVAL (CONS (EAGER (TRINARY func),CONS (arg1,CONS (arg2,CONS (arg3,NIL)))),env) =
			func (EVAL (arg1,env),EVAL (arg2,env),EVAL (arg3,env)) env
		| EVAL _ = raise Match;

	(* Section 5+10 *)
	(* this is a NORMAL function *)
	fun COND (arg1,arg2,arg3) env = 
		if NULL (EVAL (arg1,env)) NIL == NIL
		then EVAL (arg2,env)
		else EVAL (arg3,env);

	(* Section 11 *)
	(* this is a NORMAL function *)
	fun SETQ (key,value) env = 
		CONS (CONS (key,EVAL (value,env)),env);

end;
