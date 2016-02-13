(* CALCULATIONS *)
let t8a = has_vars (Add (Var, Int 2))
let t8b = not (has_vars (Add (Int 1, Int 2)))
let t8c = has_vars (Mul (Add (Var, Int 2), Mul(Int 2, Int 3)))
let t8d = not (has_vars (Add (Add (Int 1, Int 2), Mul (Int 3, Int 6))))
let t8e = has_vars (Parity (Mul (Add(Int 1, Int 3),Add (Var, Int 2))))
let t8f = has_vars (Add (Mul (Add (Mul (Add (Mul (Add (Var, Int 2), Mul(Int 2, Int 3)), Int 2), Mul(Int 2, Int 3)), Int 2), Mul(Int 2, Int 3)), Int 2))
let t8g = not (has_vars (Mul (Mul (Add (Mul (Add (Int 2, Int 2), Mul(Int 2, Int 3)), Int 2), Mul(Int 2, Int 3)), Int 2)))

let t9a = count_vars (Add (Var, Int 2)) = 1
let t9b = count_vars (Add (Int 1, Int 2)) = 0
let t9c = count_vars (Parity (Mul (Add(Var, Mul (Add(Var, Int 3),Add (Var, Mul (Add(Var, Int 3),Add (Var, Int 2))))),Add (Var, Int 2)))) = 6
let t9d = count_vars (Add (Mul (Add (Mul (Add (Mul (Add (Var, Int 2), Mul(Int 2, Int 3)), Int 2), Mul(Int 2, Int 3)), Int 2), Mul(Int 2, Int 3)), Int 2)) = 1
let t9e = count_vars (Mul (Mul (Add (Mul (Add (Int 2, Int 2), Mul(Int 2, Int 3)), Int 2), Mul(Int 2, Int 3)), Int 2)) = 0

let t10a = calc_eval (Add (Var, Int 2), 3) = 5
let t10b = calc_eval (Mul (Mul (Add (Mul (Add (Int 2, Int 2), Mul(Int 2, Int 3)), Int 2), Mul(Int 2, Int 3)), Int 2), 5) = 312
let t10c = calc_eval (Parity (Mul (Add(Var, Mul (Add(Var, Int 3),Add (Var, Mul (Add(Var, Int 3),Add (Var, Int 2))))),Add (Var, Int 2))), 4) = 0
let t10d = calc_eval (Parity (Mul (Add(Var, Mul (Add(Var, Int 3),Add (Var, Mul (Add(Var, Int 3),Add (Var, Int 2))))),Add (Var, Int 2))), 3) = 1
let t10e = calc_eval (Mul (Add(Var, Mul (Add(Var, Int 3),Add (Var, Mul (Add(Var, Int 3),Add (Var, Int 2))))),Add (Var, Int 2)), 3) = 1005

let t11a = func_of_calc (Add (Var, Int 2)) 3 = 5

let t12a = subst (Add (Var, Int 1), Mul (Var, Var)) =
                Mul (Add (Var, Int 1), Add (Var, Int 1))

let t13a = power 3 = Mul (Mul (Var, Var), Var)

let t14a = term (2, 1) = Mul(Int 2, Var)

let t15a = poly [(2, 1); (1, 4)] = Add (term (2, 1), term (1, 4))

let t16a = simplify (Add (Int 0, Var)) = Var
let t16b = simplify (Add (Int 3, Int 4)) = Int 7
let t16c = calc_eval (simplify (poly [(2, 1); (1, 0)]), 3) = 7
