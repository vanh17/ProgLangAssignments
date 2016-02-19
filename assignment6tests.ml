(* These tests assume that you have implemented the function `take` as suggested *)
let t1a = take 4 (const 3) = [3; 3; 3; 3]
let t1b = match take 4 (const (alt 3 4)) with 
          | x :: y :: z :: t :: [] -> (take 4 x, take 5 y, take 6 z, take 7 t) = (take 4 (alt 3 4), take 5 (alt 3 4), take 6 (alt 3 4), take 7 (alt 3 4))
let t1c = take 5 (const 6) = [6; 6; 6; 6; 6]
let t1d = take 0 (const 1) = []
let t1e = take 1 (const 5) = take1 (const 5) :: []
let t1f = take 1 (const 5) = [5]


let t2a = take 5 (alt 3 4) = [3; 4; 3; 4; 3]
let t2b = take 4 (alt 3 4) = [3; 4; 3; 4]
let t2c = match take 4 (alt (const 3) (const 4)) with 
          | x :: y :: z :: t :: [] -> (take 4 x, take 5 y, take 6 z, take 7 t) = (take 4 (const 3), take 5 (const 4), take 6 (const 3), take 7 (const 4))
let t2d = take 4 (alt (take 4 (alt 3 4)) (take 5 (alt 4 3))) = [[3; 4; 3; 4]; [4; 3; 4; 3; 4]; [3; 4; 3; 4]; [4; 3; 4; 3; 4]]
let t2e = match take 4 (alt (seq 3 6) (seq 0 5)) with 
          | x :: y :: z :: t :: [] -> (take 2 x, take 3 y, take 2 z, take 3 t) = ([3; 9], [0; 5; 10], [3; 9], [0; 5; 10])
let t2f = match take 3 (alt (seq 3 6) (seq 0 5)) with 
          | x :: y :: z :: [] -> (take 2 x, take 3 y, take 2 z) = ([3; 9], [0; 5; 10], [3; 9])

let t3a = take 3 (seq 2 6) = [2; 8; 14]
let t3b = take 5 (seq 0 10) = [0; 10; 20; 30; 40]
let t3c = match take 5 (alt (seq 5 6) (seq 6 5)) with
          | x1 :: x2 :: x3 :: x4 :: x5 :: [] -> (take 3 x1, take 4 x2, take 5 x3, take 6 x4, take 7 x5) = ([5; 11; 17], [6; 11; 16; 21], [5; 11; 17; 23; 29], [6; 11; 16; 21; 26; 31], [5; 11; 17; 23; 29; 35; 41])

let t4a = take 5 (from_f (fun x -> x * x)) = [1; 4; 9; 16; 25]
(* The next test ensures that the function is not called until the corresponding
   value is actually needed. *)
let t4b = try (ignore (from_f (fun _ -> raise (Failure ""))); true) with
          | _ -> false
let t4c = take 5 (from_f (fun n -> take n (seq 5 6))) = [[5]; [5; 11]; [5; 11; 17]; [5; 11; 17; 23]; [5; 11; 17;23; 29]]
let t4d = take 6 (from_f (fun x -> 0)) =[0; 0; 0; 0; 0; 0]
let t4e = match take 3 (from_f (fun x -> fun y -> x * y)) with
          | f1 :: f2 :: f3 :: [] -> (f1 2, f2 4, f3 6) = (2, 8, 18)

let t5a = take 5 (from_list [3; 5; 6]) = [3; 5; 6; 3; 5]
let t5b = take 5 (from_list (take 5 (from_f (fun n -> take n (seq 5 6))))) = [[5]; [5; 11]; [5; 11; 17]; [5; 11; 17; 23]; [5; 11; 17;23; 29]]
let t5c = match take 4 (from_list (take 3 (from_f (fun x -> fun y -> x * y)))) with
          | f1 :: f2 :: f3 :: f4 :: [] -> (f1 2, f2 2, f3 2, f4 2) = (2, 4, 6, 2)
let t5d = match take 4 (from_list (take 3 (alt (seq 5 6) (const 5)))) with
          | s1 :: s2 :: s3 :: s4 :: [] -> (take 3 s1, take 4 s2, take 5 s3, take 3 s4) = ([5; 11; 17], [5; 5; 5; 5], [5; 11; 17; 23; 29], [5; 11; 17])

let t6a = take 3 (drop 3 (seq 2 6)) = [20; 26; 32]
let t6b = take 3 (drop 4 (seq 2 6)) = [26; 32; 38]
let t6c = match take 3 (drop 5 (alt (seq 2 6) (const 5))) with
          | x :: y :: z :: [] -> (take 3 x, take 3 y, take 3 z) = ([5; 5; 5], [2; 8;  14], [5; 5; 5])
let t6d = take 3 (drop 0 (seq 2 6)) = take 3 (drop (-2) (seq 2 6))
let t6e = match take 3 (drop 5 (alt (seq 2 6) (const 5))) with
          | x :: y :: z :: [] -> (take 3 x, take 3 y, take 3 z) = match take 3 (drop 7 (alt (seq 2 6) (const 5))) with
                                                                  | x1 :: y1 :: z1 :: [] -> (take 3 x1, take 3 y1, take 3 z1)
let t6f = match take 3 (drop 4 (alt (seq 2 6) (const 5))) with
          | x :: y :: z :: [] -> (take 3 x, take 3 y, take 3 z) = match take 3 (drop 6 (alt (seq 2 6) (const 5))) with
                                                                  | x1 :: y1 :: z1 :: [] -> (take 3 x1, take 3 y1, take 3 z1)
let t6g = match take 3 (drop 5 (alt (seq 2 6) (const 5))) with
          | x :: y :: z :: [] -> (take 3 x, take 3 y, take 3 z) = match take 3 (alt (const 5) (seq 2 6)) with
                                                                  | x1 :: y1 :: z1 :: [] -> (take 3 x1, take 3 y1, take 3 z1)

let t7a = take 6 (prepend [1; 2] (const 3)) = [1; 2; 3; 3; 3; 3]
let t7b = take 6 (prepend [] (const 3)) = take 6 (const 3)
let t7c = match take 4 (prepend [alt 3 4; seq 3 4; const 3] (const (const 3))) with
          | x1 :: x2 :: x3 :: x4 :: [] -> (take 3 x1, take 3 x2, take 3 x3, take 3 x4) = (take 3 (alt 3 4), take 3 (seq 3 4), take 3 (const 3), take 3 (const 3))

let t8a = take 6 (map (fun x -> x * x) (seq 1 1)) = [1; 4; 9; 16; 25; 36]
(* The next test ensures that the function is not called until the corresponding
   value is actually needed. *)
let t8b = try (ignore (map (fun _ -> raise (Failure "")) (seq 1 1)); true) with
          | _ -> false

let t9a = take 3 (pair_up (seq 1 1)) = [(1, 2); (3, 4); (5, 6)]

let t10a = take 3 (zip2 (seq 1 2) (seq 2 3)) = [(1, 2); (3, 5); (5, 8)]

let t11a = take 4 (accum (+) 0 (seq 1 1)) = [0; 1; 3; 6]

let t12a = take 4 (filter (fun x -> x mod 2 = 0) (seq 1 1)) = [2; 4; 6; 8]

let t13a = take 3 (collect 3 (seq 1 1)) = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]]

let t14a = take 5 (flatten (collect 3 (seq 1 1))) = [1; 2; 3; 4; 5]

let t15a = take 4 (list_combos (seq 1 1) (seq 1 1)) =
                  [[(1, 1)]; [(2, 1); (1, 2)]; [(3, 1); (2, 2); (1, 3)];
                   [(4, 1); (3, 2); (2, 3); (1, 4)]]

let t16a = take 10 (list_combos_flat (seq 1 1) (seq 1 1)) =
                  [(1, 1); (2, 1); (1, 2); (3, 1); (2, 2); (1, 3);
                   (4, 1); (3, 2); (2, 3); (1, 4)]
(*
   The following test is an integration test putting together some of the things
   you've build so far. It builds a stream that produces (albeit not terribly
   efficiently) all pythagorean triples:
   https://en.wikipedia.org/wiki/Pythagorean_triple
   It does the following:
   - Starts with the sequence of natural numbers 1, 2, 3, 4, using `seq`
   - Forms all possible pairs of pairs ((a, b), c) from that sequence using
   list_combos_flat
   - Filters that list using the condition a^2 + b^2 = c^2 and the `filter` function

   Then we read out the first 5 answers.
*)
let t17a = let nats = seq 1 1 in
           let pairs = list_combos_flat (list_combos_flat nats nats) nats in
           let triples = filter (fun ((a, b), c) -> a * a + b * b = c * c) pairs
           in take 8 triples = [((4, 3), 5); ((3, 4), 5); ((8, 6), 10); ((6, 8), 10);
                                ((12, 5), 13); ((5, 12), 13); ((12, 9), 15); ((9, 12), 15)]

(*
   This integration tests shows that whenever you add all consecutive integers starting
   from 1 then the result is all the perfect squares:
*)
let t17b = let odds = seq 1 2 in
           let sums = accum (+) 0 odds in
           let squares = map (fun x -> x * x) (seq 0 1)
           in take 8 (zip2 sums squares) =
                  [(0, 0); (1, 1); (4, 4); (9, 9);
                   (16, 16); (25, 25); (36, 36); (49, 49)]
