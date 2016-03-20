exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)

(* You will need to add more cases here. *)
type exprS = NumS of float
             | BoolS of bool
             | IfS of BoolS * exprS * exprS

(* You will need to add more cases here. *)
type exprC = NumC of float
             | BoolC of bool
             | IfC of BoolC * exprC * exprC


(* You will need to add more cases here. *)
type value = Num of float
             | Bool of bool

type 'a env = (string * 'a) list
let empty = []

(* lookup : string -> 'a env -> 'a option *)
let rec lookup str env = match env with
  | []          -> None
  | (s,v) :: tl -> if s = str then Some v else lookup str tl
(* val bind :  string -> 'a -> 'a env -> 'a env *)
let bind str v env = (str, v) :: env


(*
   HELPER METHODS
   You may be asked to add methods here. You may also choose to add your own
   helper methods here.
*)
(* INTERPRETER *)

(* You will need to add cases here. *)
(* desugar : exprS -> exprC *)
let rec desugar exprS = match exprS with
  | NumS i        -> NumC i
  | BoolS i       -> BoolC i
  | IfS (cond, th, els) -> let (condC, thC, elsC) = (desugar cond, desugar th, desugar els)
                           in IfC (condC, thC, elsC)

(* You will need to add cases here. *)
(* interp : Value env -> exprC -> value *)
let rec interp env r = match r with
  | NumC i        -> Num i
  | BoolC i       -> Bool i
  | IfC (i1, i2, i3) -> match (interp env i1) with
                        | BoolC i1' -> if (i1') then interp env i2 
                                                else interp env i3
                        | _ -> raise Interp

(* evaluate : exprC -> val *)
let evaluate exprC = exprC |> interp []




(* You will need to add cases to this function as you add new value types. *)
let rec valToString r = match r with
  | Num i           -> string_of_float i
  | Bool i          -> string_of_bool i
