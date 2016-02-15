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
let t11b = func_of_calc (Mul (Mul (Add (Mul (Add (Int 2, Int 2), Mul(Int 2, Int 3)), Int 2), Mul(Int 2, Int 3)), Int 2)) 5 = 312
let t11c = func_of_calc (Parity (Mul (Add(Var, Mul (Add(Var, Int 3),Add (Var, Mul (Add(Var, Int 3),Add (Var, Int 2))))),Add (Var, Int 2)))) 4 = 0
let t11d = func_of_calc (Parity (Mul (Add(Var, Mul (Add(Var, Int 3),Add (Var, Mul (Add(Var, Int 3),Add (Var, Int 2))))),Add (Var, Int 2)))) 3 = 1
let t11e = func_of_calc (Mul (Add(Var, Mul (Add(Var, Int 3),Add (Var, Mul (Add(Var, Int 3),Add (Var, Int 2))))),Add (Var, Int 2))) 3 = 1005


let t12a = subst (Add (Var, Int 1), Mul (Var, Var)) =
                Mul (Add (Var, Int 1), Add (Var, Int 1))
let t12b = subst (Mul (Add (Var, Int 2), Mul(Int 2, Int 3)), Mul (Add (Var, Int 2), Mul(Int 2, Int 3))) =
                Mul (Add (Mul (Add (Var, Int 2), Mul(Int 2, Int 3)), Int 2), Mul(Int 2, Int 3))
let t12c = subst (subst (Mul (Add (Var, Int 2), Mul(Int 2, Int 3)), Mul (Add (Var, Int 2), Mul(Int 2, Int 3))), Mul (Add (Mul (Add (Var, Int 2), Mul(Int 2, Int 3)), Int 2), Mul(Int 2, Int 3))) =
                Mul (Add (Mul (Add (Mul (Add (Mul (Add (Var, Int 2), Mul(Int 2, Int 3)), Int 2), Mul(Int 2, Int 3)), Int 2), Mul(Int 2, Int 3)), Int 2), Mul(Int 2, Int 3))


let t13a = power (Var, 3) = Mul (Mul (Var, Var), Var)
let t13b = power (Var, 4) = Mul (Mul (Mul (Var, Var), Var), Var)
let t13c = power (power (Var, 4), 4) = Mul (Mul (Mul (Mul (Mul (Mul (Var, Var), Var), Var), Mul (Mul (Mul (Var, Var), Var), Var)), Mul (Mul (Mul (Var, Var), Var), Var)), Mul (Mul (Mul (Var, Var), Var), Var))
let t13d = power (Add (Var, Int 2), 5) = Mul (Mul (Mul (Mul (Add (Var, Int 2), Add (Var, Int 2)), Add (Var, Int 2)), Add (Var, Int 2)), Add (Var, Int 2))

let t14a = term (2, 1) = Mul(Int 2, Var)
let t14b = term (0, 1) = Int 0
let t14c = term (0, 3) = Int 0
let t14d = term (0, 0) = Int 0
let t14e = term (3, 0) = Int 3
let t14f = term (1, 0) = Int 1
let t14g = term (22, 0) = Int 22
let t14h = term (1, 1) = Var
let t14i = term (1, 3) = Mul (Mul (Var, Var), Var)
let t14j = term (3, 3) = Mul (Int 3, Mul (Mul (Var, Var), Var))


let t15a = poly [(2, 1); (1, 4)] = Add (term (2, 1), term (1, 4))
let t15b = poly [(2, 1)] = term (2, 1)
let t15c = poly [(2, 1); (0, 3); (1, 4)] = Add (term (2, 1), term (1, 4))
let t15d = poly [(2, 1); (0, 3); (0, 4)] = term (2, 1)
let t15e = poly [(0, 1); (0, 3); (0, 4)] = Int 0
let t15f = poly [] = Int 0
let t15g = poly [(2, 1); (1, 3); (1, 4); (4, 1); (5, 3); (5, 4)] = Add (Add (Add (Add (Add (term (2, 1), term (1, 3)), term (1, 4)), term (4,1)), term (5, 3)), term (5, 4))
let t15h = poly [(0, 3); (0, 4); (2, 1)] = term (2, 1)
let t15i = poly [(2, 3); (0, 3); (0, 4); (2, 1)] = Add (term (2, 3), term (2, 1))
let t15j = poly [(0, 3); (0, 4); (2, 3); (2, 1)] = Add (term (2, 3), term (2, 1))
let t15k = poly [(0, 3); (0, 4); (2, 3); (2, 1); (2, 1)] = Add (Add (term (2, 3), term (2, 1)), term (2, 1))
let t15l = poly [(5, 6); (0, 3); (0, 4); (2, 3); (2, 1); (0, 4); (0, 4); (2, 1); (0, 4)] = Add (Add (Add (term (5, 6), term (2, 3)), term (2, 1)), term (2, 1))


let t16a = simplify (Add (Int 0, Var)) = Var
let t16b = simplify (Add (Int 3, Int 4)) = Int 7
let t16c = calc_eval (simplify (poly [(2, 1); (1, 0)]), 3) = 7
let t16d = simplify (Add (Mul (Var, Var), Mul (Var, Var))) = Mul (Int 2, Mul (Var, Var))
let t16e = simplify (Add (Mul (Var, Int 2), Mul (Int 2, Var))) = Mul (Int 2, Mul (Int 2, Var))
let t16f = simplify (Add (Mul (Var, Int 2), Var)) = Mul (Int 3, Var)
let t16g = calc_eval (simplify (poly [(0, 4); (2, 3); (0, 3); (0, 4); (2, 1); (0,5)]), 2) = 20