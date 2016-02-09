(* THUNKS *)
(* This complicated test ensures you don't call the thunk too soon *)
let t1a = let f = fun () -> raise (Failure "")
          in try (try (thunk f) with Failure "" -> (fun () -> false)) ()
             with Failure "" -> true
                | _ -> false

let t1b = (thunk (fun () -> 5)) () = 5
let t1c = (thunk (fun () -> -3)) () = -3
let t1d = (thunk (fun () -> (fun x -> x * x) 3)) () = 9
let t1e = (thunk (thunk (fun () -> (fun x -> x * x) 3))) () = 9
let t1f = (thunk (thunk (thunk_of_value ((fun (x, y) -> x * y) (5, 6))))) () = 30
let t1g = (thunk (thunk_of_eval ((fun x -> x + 1), 5))) () = 6
let t1h = (thunk (thunk_of_eval ((thunk_of_eval), ((fun x -> x * x), 5)) ())) () = 25
let t1i = (thunk (thunk_of_pair ((thunk_of_eval ((fun x -> x + 1), 5)), ((thunk_of_eval ((thunk_of_eval), ((fun x -> x * x), 5)) ()))))) () = (6, 25)
let t1j = (thunk (thunk_of_pair ((fun () -> 4), (fun () -> 5)))) () = (4, 5)
let t1k = (thunk (thunk_map ((fun () -> 4), (fun x -> 2 * x)))) () = 8

let t2a = (thunk_of_value 4) () = 4
let t2b = (thunk_of_value ((fun (x, y) -> x * y) (5, 6))) () = 30
let t2c = (thunk_of_value ((thunk (fun () -> 5 + 6)) ())) () = 11
let t2d = (thunk_of_value (thunk (fun () -> 5) ())) () = 5
let t2e = (thunk_of_value ((thunk (thunk_of_eval ((thunk_of_eval), ((fun x -> x * x), 5)) ())) ())) () = 25
let t2f = (thunk_of_value ((thunk (thunk_of_pair ((thunk_of_eval ((fun x -> x + 1), 5)), ((thunk_of_eval ((thunk_of_eval), ((fun x -> x * x), 5)) ()))))) ())) () = (6, 25)
let t2g = (thunk_of_value (fun () -> ()) ()) () = ()
let t2h = (thunk_of_value ((thunk_of_value ((fun (x, y) -> x * y) (5, 6))) ())) () = 30
let t2i = (thunk_of_value ((thunk_of_value (thunk (fun () -> 5) ())) ())) () = 5
let t2g = (thunk_of_value (thunk (thunk_map ((fun () -> 4), (fun x -> 2 * x)))) ()) () = 8
let t2k = (thunk_of_value (try_thunk (fun () -> raise (Failure "hi")))) () = None
let t2l = (thunk_of_value (try_thunk (fun () -> 5 + 6))) () = Some 11

let t3a = try (try (thunk_of_eval ((fun x -> raise (Failure "")), 4))
               with Failure "" -> (fun () -> false)) ()
          with Failure "" -> true
             | _ -> false
let t3b = thunk_of_eval ((fun x -> x + 1), 5) () = 6
let t3c = (thunk_of_eval ((thunk_of_value), ((fun (x, y) -> x * y) (5, 6))) ()) () = 30
let t3d = (thunk_of_eval ((thunk_of_eval), ((fun x -> x * x), 5)) ()) () = 25
let t3e = thunk_of_eval ((thunk_of_value (thunk (thunk_map ((fun () -> 4), (fun x -> 2 * x)))) ()), ()) () = 8
let t3f = (thunk_of_eval ((thunk_of_value), ((fun (x, y) -> x * y) (5, 6))) ()) () = 30
let t3g = thunk_of_eval ((thunk_of_value (try_thunk (fun () -> raise (Failure "hi")))), ()) () = None
let t3h = thunk_of_eval ((thunk_of_value (try_thunk (fun () -> 5 + 6))), ()) () = Some 11
let t3i = thunk_of_eval (thunk (fun () -> 5), ()) () = 5
let t3j = thunk_of_eval (thunk_of_pair ((fun () -> 4), (fun () -> 5)), ()) () = (4, 5)

let t4a = try_thunk (fun () -> raise (Failure "hi")) = None
let t4b = try_thunk (fun () -> 5 + 6) = Some 11
let t4c = try_thunk ((thunk_of_eval ((thunk_of_eval), ((fun x -> x * x), 5)) ())) = Some 25
let t4d = try_thunk (fun () -> raise (Failure "try_thunk")) = None
let t4e = try_thunk (thunk_of_eval ((thunk_of_value (thunk (thunk_map ((fun () -> 4), (fun x -> 2 * x)))) ()), ())) = Some 8
let t4f = try_thunk ((thunk_of_eval ((thunk_of_eval), ((fun x -> fun () -> x + 2) 5, ())) ())) = Some 7
let t4g = try_thunk (thunk_of_eval (thunk_of_pair ((fun () -> 4), (fun () -> 5)), ())) = Some (4, 5)
let t4h = try_thunk (fun () -> try_thunk (fun () -> raise (Failure "hi"))) = Some None
let t4i = try_thunk (fun () -> try_thunk (fun () -> try_thunk (fun () -> raise (Failure "hi")))) = Some (Some None)
let t4j = try_thunk (fun () -> ()) = Some ()

let t5a = let f = fun () -> raise (Failure "")
          in try (try (thunk_of_pair (f, f)) with Failure "" -> (fun () -> (1, 1))) () =
                  (0, 0)
             with Failure "" -> true
                | _ -> false
let t5b = thunk_of_pair ((fun () -> 4), (fun () -> 5)) () = (4, 5)
let t5c = thunk_of_pair ((thunk_of_eval ((fun x -> x + 1), 5)), ((thunk_of_eval ((thunk_of_eval), ((fun x -> x * x), 5)) ()))) () = (6, 25)
let t5d = thunk_of_pair ((thunk_of_value ((thunk (fun () -> 5 + 6)) ())), (thunk_of_value ((fun (x, y) -> x * y) (5, 6)))) () = (11, 30)
let t5e = thunk_of_pair ((fun () -> try_thunk (fun () -> raise (Failure "hi"))), (fun () -> 5)) () = (None, 5)
let t5h = thunk_of_pair ((thunk_of_eval ((thunk_of_value), ((fun (x, y) -> x * y) (5, 6))) ()), (fun () -> 5)) () = (30, 5)
let t5f = thunk_of_pair ((thunk_of_eval ((thunk_of_eval), ((fun x -> x * x), 5)) ()), ((thunk_of_eval ((thunk_of_eval), ((fun x -> x * x), 5)) ()))) () = (25, 25)
let t5g = thunk_of_pair ((fun () -> ()), (thunk_of_value ((fun (x, y) -> x * y) (5, 6)))) () = ((), 30)

let t6a = let f = fun () -> raise (Failure "")
          in try (try thunk_map (f, f)
                  with Failure "" -> (fun () -> false)) ()
             with Failure "" -> true
                | _ -> false
let t6b = thunk_map ((fun () -> 4), (fun x -> 2 * x)) () = 8
let t6c = thunk_map ((thunk (thunk_of_pair ((thunk_of_value ((thunk (fun () -> 5 + 6)) ())), (thunk_of_value ((fun (x, y) -> x * y) (5, 6)))))), (fun (x, y) -> y * x)) () = 330
let t6d = thunk_map ((fun () -> ()), (thunk_of_pair ((thunk_of_eval ((thunk_of_value), ((fun (x, y) -> x * y) (5, 6))) ()), (fun () -> 5)))) () = (30, 5)
let t6e = thunk_map ((fun () -> ()), (thunk_of_pair ((fun () -> ()), (thunk_of_value ((fun (x, y) -> x * y) (5, 6)))))) () = ((), 30)
let t6g = thunk_map ((fun () -> ()), (fun () -> ((), ()))) () = ((), ())
let t6h = thunk_map ((fun () -> ()), (thunk_of_pair ((fun () -> try_thunk (fun () -> raise (Failure "hi"))), (fun () -> 5)))) () = (None, 5)
let t6i = thunk_map ((fun () -> thunk_of_pair ((thunk_of_eval ((thunk_of_value), ((fun (x, y) -> x * y) (5, 6))) ()), (fun () -> 5))), try_thunk) () = Some (30, 5)
let t6j = thunk_map ((fun () -> thunk_map ((fun () -> 4), (fun x -> 2 * x)) ()), (fun x -> 2 * x)) () = 16
let t6k = (thunk_map ((fun () -> thunk_of_pair ((fun () -> 4), (fun () -> 5))), thunk) ()) () = (4, 5)

let t7a = let f = fun () -> raise (Failure "")
          in try (try thunk_of_list [f; f]
                  with Failure "" -> (fun () -> [])) () = []
             with Failure "" -> true
                | _ -> false
let t7b = let f = fun () -> 5
          in thunk_of_list [f; f] () = [5; 5]
let t7c = let f = thunk_map ((fun () -> thunk_of_pair ((fun () -> 4), (fun () -> 5))), thunk) ()
          in thunk_of_list [f; f; f] () = [(4, 5); (4, 5) ; (4, 5)]
let t7d = let f = fun () -> try_thunk (fun () -> try_thunk (fun () -> raise (Failure "hi")))
          in thunk_of_list [f; f] () = [Some None; Some None]
let t7e = let f = thunk_map ((fun () -> 4), (fun x -> 2 * x))
          in thunk_of_list [f; f] () = [8; 8]
let t7f = let f = thunk_of_eval ((fun x -> x + 1), 5)
          in thunk_of_list [f; f] () = [6; 6]
let t7g = let f = let g = thunk_of_eval ((fun x -> x + 1), 5)
                  in thunk_of_list [g; g]
          in thunk_of_list [f; f] () = [[6; 6]; [6; 6]]
let t7h = let f = thunk_map ((fun () -> ()), (fun () -> ((), ())))
          in thunk_of_list [f; f] () = [((), ()); ((), ())]
let t7i = let f = let t = let g = thunk_of_eval ((fun x -> x + 1), 5)
                          in thunk_of_list [g; g]
                  in thunk_of_list [t; t]
          in thunk_of_list [f; f] () = [[[6; 6]; [6; 6]]; [[6; 6]; [6; 6]]] 

let t8a = insert (empty, "foo", 3) = [("foo", 3)]

let t9a = has ([("foo", 2)], "foo") = true
let t9b = has ([], "foo") = false
let t9c = has ([("foo", 2)], "foe") = false

let t10a = lookup ([("bar", 3); ("foo", 2)], "bar") = 3
let t10b = try (lookup ([("bar", 3); ("foo", 2)], "baz"); false)
           with Not_found -> true
(* In the following test the search should fail because your code
   should stop looking after baz, since "baz" > "bar".
   This is of course not a "proper" table, but it is a good test that
   your code behaves properly. *)
let t10c = try (lookup ([("baz", 3); ("bar", 2)], "bar"); false)
           with Not_found -> true

let t11a = lookup_opt ([("bar", 3); ("foo", 2)], "bar") = Some 3
(* Again the search should be stopping after "foo" *)
let t11b = lookup_opt ([("foo", 2); ("bar", 3)], "bar") = None

let t12a = delete ([("bar", 3); ("baz", 1); ("foo", 2)], "bar") = [("baz", 1); ("foo", 2)]
let t12b = delete (delete (delete ([("bar", 3); ("baz", 1); ("foo", 2)], "bar"), "baz"), "foo") = []

let t13a = keys [("bar", 3); ("foo", 2)] = ["bar"; "foo"]
let t13b = keys [] = []
let t13c = keys (delete ([("bar", 3); ("baz", 1); ("foo", 2)], "bar")) = ["baz"; "foo"]

let t14a = is_proper [("bar", 3); ("foo", 2)] = true
let t14b = is_proper [("bar", 3); ("a", 2)] = false
let t14c = is_proper [("a", 5); ("bar", 3); ("", 2)] = false
