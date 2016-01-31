(* Programming Languages, Assignment 3 *)
(*
   You should write your functions in this file.
   You should NOT specify the types of your functions. Let the system determine
   them for you.
   Write your code right below the corresponding comment describing the
   function you are asked to write.
*)

(* ----------------------------------------
            ROCK PAPER SCISSORS
   ---------------------------------------- *)

(*
   In the first set of exercises we will be describing in OCAML
   the game rock-paper-scissors. If somehow you are not familiar with the
   game, read it in wikipedia.
   We will specify some custom types for the game:
   - A "shape" is one of the possible hand shapes that a player can make.
   - A "check" is a pair of the two shapes that the two players are supposed
         to have made.
   - The "result" of a check is whether it was a tie, or whether the first
         or the second player won. For instance the result of (Rock, Paper)
         should be that the second player won.
   - A "game" is a list of "checks" that are supposed to happen in order.
   - A "valid game" is a game that could actually occur: Players only continue
         to perform "checks" if they run in a tie. For instance a valid game
         would be: [(Rock, Rock), (Rock, Paper)]
         But this is not valid: [(Rock, Rock), (Rock, Paper), (Paper, Scissors)]
         because the 3rd check should not be happening.
         Also this is not valid: [(Rock, Rock), (Paper, Paper)]
         because the game cannot end with ties.
         In other words a "valid game" would consist of a sequence of tied checks
         followed by one non-tied check, and nothing after it.
   - A game, even a non-valid one, can be "played" as follows:
      - Look at each check in the list in order. Return the first result that is
            not a Tie.
      - If all checks in the game are Ties, return a Tie.
   - A "play" is a list of shapes. They represent the intended "plays" of the player.
         For example [Rock, Rock, Paper] means that the player will play Rock on
         the first check, Rock on the second check and Paper on the third check.
*)
type shape = Rock | Paper | Scissors
type check = shape * shape
type result = Tie | FstWin | SndWin
type game = check list
type play = shape list

(*
   Write a function `result` that takes as input a check and
   returns the result of that check.
   Type: check -> result
*)
let result check1 =
    match check1 with
    | (s1, s2) -> if s1 = s2 then Tie
                 else
                    if (s1 = Rock && s2 = Scissors)
                        || (s1 = Paper && s2 = Rock)
                        || (s1 = Scissors && s2 = Paper)
                    then FstWin
                    else SndWin


(*
   Write a function `is_tie` that takes as input a check and returns
   whether the check's result is a tie.
   Type: check -> bool
*)
let is_tie check1 = result check1 = Tie



(*
   Write a function `game_from_plays` that takes as input two plays (correspoding
   to the intended plays of the two players) and creates a game by combining them,
   representing how they would have been played. If one play is longer than the
   other, stop at the shortest one.
   Type: play * play -> game
*)
let rec game_from_plays (x, y) =
        match (x, y) with
        | ([], _) -> []
        | (_, []) -> []
        | (x1 :: xrest, y1 :: yrest) -> if (x1 = Rock || x1 = Paper || x1 = Scissors)
                                           && (y1 = Rock || y1 = Paper || y1 = Scissors)
                                        then (x1, y1) :: game_from_plays (xrest, yrest)
                                        else raise (Failure "game_from_plays")


(*
   Write a function `valid_game` that takes as input a game and determines if it is
   a valid game as described above.
   Type: game -> bool
*)
let rec valid_game game1 =
    match game1 with
    | [] -> true
    | x :: rest -> if is_tie x
                   then (rest != []) && valid_game rest
                   else rest = [] 


(*
   Write a function `play_game` that plays the game as described above.
   Type: game -> result
*)
let play_game game1 =
        if valid_game game1
        then let rec winner trial =
                 match trial with
                 | [] -> Tie
                 | x :: rest -> if is_tie x
                                then winner rest
                                else result x
              in winner game1
        else let rec invalid trial =
                 match trial with
                 | [] -> Tie
                 | x :: rest -> if is_tie x
                                then invalid rest
                                else result x
             in invalid game1

(* --------------------------------------
            TEMPERATURES
   -------------------------------------- *)

(*
   In this section we write functions to work flexibly with temperatures.
   A value of type "temp" is effectively a number 'tagged' with a C or F
   depending on if it is meant to be Celsius or Fahrenheit.
   The conversion between the two is the familiar formula: F = 1.8 * C + 32
*)
type temp = C of float | F of float

(*
   Write a function `to_f` that takes as input a value of type "temp" and
   returns the temperature measured in Fahrenheit.
   Note: The operators for floating point arithmetic have a dot following
   them to distinguish from the integer ones. For example "2.1 +. 5.2"
   Type: temp -> float
*)
let to_f t =
        match t with
        | F x -> x
        | C x -> 1.8 *. x +. 32.0

(*
   Write a function `temp_compare` that takes as input a pair of temperatures and
   "compares" them, returning 1 if the first temperature is higher, 0 if they are
   equal and -1 if the second temperature is higher.
   Type: temp * temp -> int
*)
let temp_compare (t1, t2) =
        let (f1, f2) = (to_f t1, to_f t2)
        in if f1 = f2 then 0
           else
               if f1 < f2 then -1
               else 1


(*
   Write a function `string_of_temp` that takes as input a temperature and
   returns a string representing that temperature. For instance 23.2 Fahrenheit
   should print as "23.2F" while 23.2 Celcius as "23.2C". Look in the Pervasives
   module in the string conversions section for a function converting floats
   to strings.
   Type: temp -> string
*)
let string_of_temp t =
        match t with
        | F x -> string_of_float x ^ "F"
        | C x -> string_of_float x ^ "C"


(*
   Write a function `max_temp` that takes as input a list of temperatures and
   returns the largest one. It should raise an exception `Failure "max_temp"`
   if the list is empty.
   Type: temp list -> temp
*)
let rec max_temp tlst =
        match tlst with
        | [] -> raise (Failure "max_temp")
        | x :: [] -> x
        | x :: rest -> let nt = max_temp rest
                       in if temp_compare (x, nt) = 1
                          then x
                          else nt

(*
   Write a function `max_temp2` that behaves like `max_temp` but where all the
   recursive calls are tail calls. You will likely need to define an auxiliary
   function and use state recursion.
*)
let max_temp2 tlst =
        match tlst with
        | [] -> raise (Failure "max_temp2")
        | x :: rest -> let rec find_max (t, lst) =
                           match lst with
                           | [] -> t
                           | t1 :: t1rest -> if temp_compare (t, t1) = -1
                                           then find_max (t1, t1rest)
                                           else find_max (t, t1rest)
                       in find_max (x, rest)
