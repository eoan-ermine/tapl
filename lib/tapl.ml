type info = FI of string * int * int | UNKNOWN;;

let dummyinfo = UNKNOWN;;

type term =
	TmTrue of info
	| TmFalse of info
	| TmIf of info * term * term * term
	| TmZero of info
	| TmSucc of info * term
	| TmPrev of info * term 
	| TmIsZero of info * term
;;

let rec isnumericval t = match t with
	TmZero(_) -> true
	| TmSucc(_, t1) -> isnumericval t1
	| _ -> false
;;

let isval t = match t with
	TmTrue(_) -> true
	| TmFalse(_) -> true
	| t when isnumericval t -> true
	| _ -> false
;;

exception NoRuleApplies;;

let rec eval1 t = match t with
	TmIf(_, TmTrue(_), t2, _) ->
		t2
	| TmIf(_, TmFalse(_), _, t3) ->
		t3
	| TmIf(fi, t1, t2, t3) ->
		let t1' = eval1 t1 in
		TmIf(fi, t1', t2, t3)
	| TmSucc(fi, t1) ->
		let t1' = eval1 t1 in
		TmSucc(fi, t1')
	| TmPrev(_, TmZero(_)) ->
		TmZero(dummyinfo)
	| TmPrev(_, TmSucc(_, nv1)) when (isnumericval nv1) ->
		nv1
	| TmPrev(fi, t1) ->
		let t1' = eval1 t1 in
		TmPrev(fi, t1')
	| TmIsZero(_, TmZero(_)) ->
		TmTrue(dummyinfo)
	| TmIsZero(_, TmSucc(_, nv1)) when (isnumericval nv1) ->
		TmFalse(dummyinfo)
	| TmIsZero(fi, t1) ->
		let t1' = eval1 t1 in
		TmIsZero(fi, t1')
	| _ ->
		raise NoRuleApplies
;;

let rec eval t =
	try let t' = eval1 t
		in eval t'
	with NoRuleApplies -> t
;;

let rec evalBig1 t = match t with
	v when (isval v) ->
		v
	| TmIf(_, t1, t2, t3) ->
		(match evalBig1 t1 with
			TmTrue(_) -> evalBig1 t2
			| TmFalse(_) -> evalBig1 t3
			| _ -> raise NoRuleApplies)
	| TmSucc(fi, t1) ->
		(match evalBig1 t1 with
			t when isnumericval (evalBig1 t) -> TmSucc(fi, evalBig1 t)
			| _ -> raise NoRuleApplies)
	| TmPrev(_, t1) ->
		(match evalBig1 t1 with
			TmZero(_) -> TmZero(dummyinfo)
			| TmSucc(_, t1) when isnumericval (evalBig1 t1) -> evalBig1 t1
			| _ -> raise NoRuleApplies)
	| TmIsZero(_, t1) ->
		(match evalBig1 t1 with
			TmZero(_) -> TmTrue(dummyinfo)
			| TmSucc(_, t1) when isnumericval t1 -> TmFalse(dummyinfo)
			| _ -> raise NoRuleApplies)
	| _ ->
		raise NoRuleApplies
;;

let rec evalBig t =
	try let t' = evalBig1 t
		in evalBig t'
	with NoRuleApplies -> t
;;
