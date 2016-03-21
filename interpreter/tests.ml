open Types

(* You can test expressions of type resultS or resultC and how they are evaluated *)
(* These will only work once you have compiled types.ml *)

(* This is the one kind of test you can write. *)
let t0a = evaluate (NumC 2.3) = Num 2.3

(* You can also use interp directly to specify a custom environment. *)
let t0b = let env1 = bind "x" (Num 3.1) empty
          in interp env1 (BoolC false) = Bool false

(* You can also test desugar to make sure it behaves properly. *)
let t0c = desugar (NumS 2.3) = NumC 2.3

(* Or you can combine with evaluate to get to the final value. *)
let t0d = evaluate (desugar (NumS 2.3)) = Num 2.3

let t0e = evaluate (BoolC true) = Bool true

let t0f = evaluate (BoolC false) = Bool false

let tog = let env1 = bind "x" (Bool true) empty
          in interp env1 (BoolC true) = Bool true

let t0h = desugar (BoolS false) = BoolC false

let t0i = evaluate (desugar (BoolS true)) = Bool true



let t1a = evaluate (IfC (BoolC true, NumC 2.3, NumC 3.2)) = Num 2.3

(* You can also use interp directly to specify a custom environment. *)
let t1b = let env1 = bind "x" (Num 3.1) empty
          in interp env1 (IfC (BoolC false, BoolC false, BoolC true)) = Bool true

(* You can also test desugar to make sure it behaves properly. *)
let t1c = desugar (IfS (BoolS false, NumS 2.3, NumS 4.5)) = IfC (BoolC false, NumC 2.3, NumC 4.5)

(* Or you can combine with evaluate to get to the final value. *)
let t1d = evaluate (desugar (IfS (BoolS false, NumS 2.3, NumS 4.5))) = Num 4.5

let t1e = evaluate (desugar (IfS (IfS (BoolS false, BoolS false, BoolS true), BoolS false, BoolS true))) = Bool false

let t1f = try (desugar (IfS (NumS 2.3, BoolS true, BoolS false)); false) with Desugar "desugarErr" -> true
                                                                              | _ -> false

let t1g = try (evaluate (IfC (NumC 2.3, BoolC true, BoolC false)); false) with Interp "interpErr" -> true
                                                                               | _ -> false

let t1h = let env1 = bind "x" (Num 3.1) empty
          in try ((interp env1 (IfC (NumC 3.5, BoolC false, BoolC true))); false) with Interp "interpErr" -> true
                                                                                       | _ -> false




