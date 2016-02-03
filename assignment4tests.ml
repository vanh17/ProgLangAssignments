(* THUNKS *)
(* This complicated test ensures you don't call the thunk too soon *)
let t1a = let f = fun () -> raise (Failure "")
          in try (try (thunk f) with Failure "" -> (fun () -> false)) ()
             with Failure "" -> true
                | _ -> false

let t1b = (thunk (fun () -> 5)) () = 5

let t2a = (thunk_of_value 4) () = 4

let t3a = try (try (thunk_of_eval ((fun x -> raise (Failure "")), 4))
               with Failure "" -> (fun () -> false)) ()
          with Failure "" -> true
             | _ -> false
let t3b = thunk_of_eval ((fun x -> x + 1), 5) () = 6

let t4a = try_thunk (fun () -> raise (Failure "hi")) = None

let t5a = let f = fun () -> raise (Failure "")
          in try (try (thunk_of_pair (f, f)) with Failure "" -> (fun () -> (1, 1))) () =
                  (0, 0)
             with Failure "" -> true
                | _ -> false
let t5b = thunk_of_pair ((fun () -> 4), (fun () -> 5)) () = (4, 5)

let t6a = let f = fun () -> raise (Failure "")
          in try (try thunk_map (f, f)
                  with Failure "" -> (fun () -> false)) ()
             with Failure "" -> true
                | _ -> false
let t6b = thunk_map ((fun () -> 4), (fun x -> 2 * x)) () = 8

let t7a = let f = fun () -> raise (Failure "")
          in try (try thunk_of_list [f; f]
                  with Failure "" -> (fun () -> [])) () = []
             with Failure "" -> true
                | _ -> false
let t7b = let f = fun () -> 5
          in thunk_of_list [f; f] () = [5; 5]

