(* Add your own tests. Make sure to pay attention to edge cases. *)
let t1a = order (2, 5, 3) == (2, 3, 5)
let t1b = order (5, 3, 2) == (2, 3, 5)

let t2a = distance (6, 3) == 3

let t3a = greeting (23, "Pete") == "Greetings Pete, you are 23 years old!"

let t4a = greeting2 (0, "Jackson") == "Greetings Jackson, you are not born yet!"

let t5a = tooShort (4, "tree") == false

let t6a = totalLength ("you", "me") == 5

let t7a = orderedByLengh ("long", "one", "at") == false

let t8a = prodInRange (3, 5) == true
