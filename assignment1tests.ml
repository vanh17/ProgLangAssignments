(* Add your own tests. Make sure to pay attention to edge cases. *)
let t1a = fixLastTwo (5, 1, 2) = (5, 1, 2)
let t1b = fixLastTwo (5, 2, 1) = (5, 1, 2)
let t1c = fixLastTwo (5, 0, 0) = (5, 0, 0)

let t2a = order (2, 5, 3) = (2, 3, 5)
let t2b = order (5, 3, 2) = (2, 3, 5)
let t2c = order (5, 6, 3) = (3, 5, 6)
let t2d = order (3, 4, 2) = (2, 3, 4)

let t3a = distance (6, 3) = 3
let t3b = distance (3, 6) = 3
let t3c = distance (5, 5) = 0
let t3d = distance (0, 0) = 0
let t3e = distance (-5, -6) = 1


let t4a = greeting (23, "Pete") = "Greetings Pete, you are 23 years old!"
let t4b = greeting (0, "Hoang") = "Greetings Hoang, you are 0 years old!"

let t5a = greeting2 (0, "Jackson") = "Greetings Jackson, you are not born yet!"
let t5b = greeting2 (1, "Jackson") = "Greetings Jackson, you are a youngster!"
let t5c = greeting2 (20, "Jackson") = "Greetings Jackson, you are a youngster!"
let t5d = greeting2 (21, "Jackson") = "Greetings Jackson, you are young at heart!"
let t5c = greeting2 (15, "Jackson") = "Greetings Jackson, you are a youngster!"
let t5d = greeting2 (45, "Jackson") = "Greetings Jackson, you are young at heart!"

let t6a = tooShort (4, "tree") = false
let t6b = tooShort (5, "tree") = true
let t6c = tooShort (5, "treee") = false
let t6d = tooShort (6, "tree") = true
let t6e = tooShort (0, "") = false
let t6f = tooShort (4, "") = true
let t6g = tooShort (3, "tree") = false

let t7a = totalLength ("you", "me") = 5
let t7b = totalLength ("", "me") = 2
let t7c = totalLength ("you", "") = 3
let t7d = totalLength ("", "") = 0

let t8a = orderedByLength ("long", "one", "at") = false
let t8b = orderedByLength ("this", "is", "false") = false
let t8c = orderedByLength ("it", "is definitely", "false") = false
let t8d = orderedByLength ("", "", "") = true
let t8e = orderedByLength ("same", "same", "same") = true
let t8f = orderedByLength ("short", "longer", "longer") = true
let t8g = orderedByLength ("short", "short", "longer") = true

let t9a = prodInRange (3, 5) == true
let t9b = prodInRange (3, 6) == true
let t9c = prodInRange (3, 7) == false
let t9d = prodInRange (0, 5) == false
let t9e = prodInRange (10, 0) == false
let t9f = prodInRange (20, 1) == true
let t9g = prodInRange (20, 2) == false
let t9h = prodInRange (3, 20) == false 

