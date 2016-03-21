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

let t1f = desugar (IfS (NumS 2.3, BoolS true, BoolS false)) = IfC (NumC 2.3, BoolC true, BoolC false)

let t1f' = try (evaluate (desugar (IfS (NumS 2.3, BoolS true, BoolS false))); false) with Interp "interpErr" -> true
                                                                                          | _ -> false

let t1g = try (evaluate (IfC (NumC 2.3, BoolC true, BoolC false)); false) with Interp "interpErr" -> true
                                                                               | _ -> false

let t1h = let env1 = bind "x" (Num 3.1) empty
          in try ((interp env1 (IfC (NumC 3.5, BoolC false, BoolC true))); false) with Interp "interpErr" -> true
                                                                                       | _ -> false

let t2a = desugar (NotS (BoolS false)) = IfC (BoolC false, BoolC false, BoolC true)

(* You can also use interp directly to specify a custom environment. *)
let t2b = try (evaluate (desugar (NotS (NumS 2.5))); false) with Interp "interpErr" -> true
                                                                 | _ -> false

(* You can also test desugar to make sure it behaves properly. *)
let t2c = desugar (NotS (IfS (BoolS false, BoolS false, BoolS true))) = IfC (IfC (BoolC false, BoolC false, BoolC true), BoolC false, BoolC true)

(* Or you can combine with evaluate to get to the final value. *)
let t2d = evaluate (desugar (NotS (IfS (BoolS false, BoolS false, BoolS true)))) = Bool false

let t3a = desugar (OrS (BoolS false, BoolS true)) = IfC (BoolC false, BoolC true, IfC (BoolC true, BoolC true, BoolC false))

(* You can also use interp directly to specify a custom environment. *)
let t3b = try (evaluate (desugar (OrS (NumS 2.5, BoolS true))); false) with Interp "interpErr" -> true
                                                                            | _ -> false

(*
let t3c = try (evaluate (desugar (OrS (BoolS true, NumS 2.5))); false) with Interp "interpErr" -> true
                                                                            | _ -> false
*)
(* Or you can combine with evaluate to get to the final value. *)
let t3d = evaluate (desugar (OrS (IfS (BoolS false, BoolS false, BoolS true), IfS (BoolS true, BoolS true, BoolS false)))) = Bool true

let t3e = try (evaluate (desugar (OrS (BoolS false, NumS 2.5))); false) with Interp "interpErr" -> true
                                                                             | _ -> false

let t3f = desugar (OrS (NumS 2.5, BoolS true)) = IfC (NumC 2.5, BoolC true, IfC (BoolC true, BoolC true, BoolC false))

let t3g = desugar (OrS (NumS 2.5, IfS (BoolS true, NumS 0.0, BoolS false))) = IfC (NumC 2.5, BoolC true, IfC (IfC (BoolC true, NumC 0.0, BoolC false), BoolC true, BoolC false))

let t4a = desugar (AndS (BoolS false, BoolS true)) = IfC (BoolC false, IfC (BoolC true, BoolC true, BoolC false), BoolC false)

(* You can also use interp directly to specify a custom environment. *)
let t4b = try (evaluate (desugar (AndS (NumS 2.5, BoolS true))); false) with Interp "interpErr" -> true
                                                                             | _ -> false

(* Or you can combine with evaluate to get to the final value. *)
let t4c = evaluate (desugar (AndS (IfS (BoolS false, BoolS false, BoolS true), IfS (BoolS true, BoolS true, BoolS false)))) = Bool true

(*
let t4d = try (evaluate (desugar (AndS (BoolS false, NumS 2.5))); false) with Interp "interpErr" -> true
                                                                              | _ -> false
*)

let t4f = desugar (AndS (NumS 2.5, BoolS true)) = IfC (NumC 2.5, IfC (BoolC true, BoolC true, BoolC false), BoolC false)

let t4g = desugar (AndS (NumS 2.5, IfS (BoolS true, NumS 0.0, BoolS false))) = IfC (NumC 2.5, IfC (IfC (BoolC true, NumC 0.0, BoolC false), BoolC true, BoolC false), BoolC false)

let t4h = try (evaluate (desugar (AndS (BoolS true, NumS 2.5))); false) with Interp "interpErr" -> true
                                                                             | _ -> false





