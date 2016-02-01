let t1a = result (Rock, Paper) = SndWin
let t1b = result (Paper, Rock) = FstWin
let t1c = result (Rock, Scissors) = FstWin
let t1d = result (Scissors, Paper) = FstWin
let t1e = result (Scissors, Rock) = SndWin
let t1f = result (Paper ,Scissors) = SndWin
let t1g = result (Rock, Rock) = Tie
let t1h = result (Paper, Paper) = Tie
let t1i = result (Scissors, Scissors) = Tie

let t2a = is_tie (Rock, Paper) = false
let t2b = is_tie (Paper, Rock) = false
let t2c = is_tie (Rock, Scissors) = false
let t2d = is_tie (Scissors, Paper) = false
let t2e = is_tie (Scissors, Rock) = false
let t2f = is_tie (Paper ,Scissors) = false
let t2g = is_tie (Rock, Rock) = true
let t2h = is_tie (Paper, Paper) = true
let t2i = is_tie (Scissors, Scissors) = true

let t3a = game_from_plays ([Rock; Paper; Rock], [Scissors; Rock; Rock]) =
               [(Rock, Scissors); (Paper, Rock); (Rock, Rock)]
let t3b = game_from_plays ([Rock; Paper], [Scissors; Rock; Rock]) =
               [(Rock, Scissors); (Paper, Rock)]
let t3c = game_from_plays ([Rock; Paper; Rock], [Scissors; Paper]) =
               [(Rock, Scissors); (Paper, Paper)]
let t3d = game_from_plays ([Rock; Paper], [Scissors; Rock; Rock]) =
               game_from_plays ([Rock; Paper; Rock], [Scissors; Rock])
let t3e = game_from_plays ([], [Scissors; Rock; Rock]) = []
let t3f = game_from_plays ([], []) = []
let t3g = game_from_plays ([Scissors; Rock; Rock], []) = []


let t4a = valid_game [(Rock, Scissors)] = true
let t4b = valid_game [(Rock, Rock); (Rock, Scissors)] = true
let t4c = valid_game [(Rock, Scissors); (Paper, Rock); (Rock, Rock)] = false


let t5a = play_game [(Rock, Rock); (Scissors, Rock)] = SndWin
let t5b = play_game [(Rock, Rock); (Rock, Scissors)] = FstWin
let t5c = play_game [(Scissors, Rock); (Rock, Rock); (Rock, Scissors)] = SndWin
let t5d = play_game [(Rock, Rock); (Rock, Scissors); (Rock, Rock)] = FstWin
let t5e = play_game [] = Tie
let t5f = play_game [(Rock, Rock); (Scissors, Scissors); (Paper, Paper)] = Tie


let t6a = to_f (F 2.3) = 2.3
let t6b = to_f (F 3.2) = 3.2
let t6c = to_f (C 0.0) = 32.0
let t6d = to_f (C (-.1.0)) = 30.2
let t6d = to_f (F (-.1.0)) = -.1.0
let t6e = to_f (C 1.0) = 33.8

let t7a = temp_compare (F 2.3, F 4.5) = -1
let t7b = temp_compare (F 2.3, C 4.5) = -1
let t7c = temp_compare (C 2.3, F 4.5) = 1
let t7d = temp_compare (F 2.3, F 0.0) = 1
let t7e = temp_compare (F 2.3, F 2.3) = 0
let t7f = temp_compare (F 32.0, C (-.0.0)) = 0

let t8a = string_of_temp (C 2.3) = "2.3C"
let t8b = string_of_temp (F 2.3) = "2.3F"
let t8c = string_of_temp (F (to_f (C 0.0))) = "32.F"
let t8d = string_of_temp (F (to_f (C 1.0))) = "33.8F"
let t8e = string_of_temp (F (-.2.3)) = "-2.3F"


let t9a = max_temp [F 2.1; C 2.1] = C 2.1
let t9b = max_temp [C 2.1; C 2.1; C 3.2] = C 3.2
let t9c = max_temp [F 2.1; F 2.0; F 3.0] = F 3.0
let t9d = max_temp [C 2.1; C 2.3; C 100.0] = C 100.0
let t9e = max_temp [F 2.1; F 3.5; C 2.5] = C 2.5
let t9f = max_temp [F 2.1; C (-.10.0)] = C (-.10.0)
let t9g = max_temp [F 32.0; C (-.1.0)] = F 32.0
let t9h = try (max_temp []; false)  with
            | Failure "max_temp" -> true
            | _ -> false



let t10a = max_temp2 [F 2.1; C 2.1] = C 2.1
let t10b = max_temp2 [C 2.1; C 2.1; C 3.2] = C 3.2
let t10c = max_temp2 [F 2.1; F 2.0; F 3.0] = F 3.0
let t10d = max_temp2 [C 2.1; C 2.3; C 100.0] = C 100.0
let t10e = max_temp2 [F 2.1; F 63.5; C 2.5] = F 63.5
let t10f = max_temp2 [F 2.1; C (-.10.0)] = C (-.10.0)
let t10g = max_temp2 [F 32.0; C (-.1.0)] = F 32.0
let t10h = try (max_temp2 []; false)  with
            | Failure "max_temp2" -> true
            | _ -> false

let t11a = max_temp [F 2.1; C 2.1] = max_temp2 [F 2.1; C 2.1]
let t11b = max_temp [C 2.1; C 2.1; C 3.2] = max_temp2 [C 2.1; C 2.1; C 3.2]
let t11c = max_temp [F 2.1; F 2.0; F 3.0] = max_temp2 [F 2.1; F 2.0; F 3.0]
let t11d = max_temp [C 2.1; C 2.3; C 100.0] = max_temp2 [C 2.1; C 2.3; C 100.0]
let t11e = max_temp [F 2.1; F 3.5; C 2.5] = max_temp2 [F 2.1; F 3.5; C 2.5]
let t11f = max_temp [F 2.1; C (-.10.0)] = max_temp2 [F 2.1; C (-.10.0)]
let t11g = max_temp [F 32.0; C (-.1.0)] = max_temp2 [F 32.0; C (-.1.0)]
