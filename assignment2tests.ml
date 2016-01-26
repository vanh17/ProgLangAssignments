let t1a = getnth (3, ["hi"; "there"; "you"]) = "you"
let t1b = getnth (2, ["hi"; "there"; "you"]) = "there"
let t1c = getnth (1, ["hi"; "there"; "you"]) = "hi"
let t1d = try (getnth (0, ["hi"; "there"]); false)  with
            | Failure "getnth" -> true
            | _ -> false
let t1e = try (getnth (3, ["hi"; "there"]); false)  with
            | Failure "getnth" -> true
            | _ -> false
let t1f = try (getnth (1, []); false)  with
            | Failure "getnth" -> true
            | _ -> false
let t1g = try (getnth (0, []); false)  with
            | Failure "getnth" -> true
            | _ -> false
let t1h = try (getnth (5, ["hi"; "there"]); false)  with
            | Failure "getnth" -> true
            | _ -> false
let t1z = (getnth (1, ["hi"; "there"; "you"])
	      ^getnth (2, ["hi"; "there"; "you"])
	      ^getnth (3, ["hi"; "there"; "you"])) 
        = "hithereyou"

let t2a = lookup ("you", []) = None;;
let t2b = lookup ("you", [("him", 2); ("you", 3)]) = Some 3
let t2c = lookup ("you", [("yoU",2)]) = None
let t2d = lookup ("you", [("you", 2); ("you", 3)]) = Some 2
let t2e = lookup ("", [("him", 2); ("you", 3)]) = None
let t2f = lookup ("", [("him", 2); ("you", 3); ("", 3)]) = Some 3
let t2f = lookup ("", [("  ", 2); ("you", 3); (" ", 3)]) = None

let t3a = inPairs [1; 2; 3; 4; 5] = [(1, 2); (3, 4)]
let t3b = inPairs [1; 2; 3; 4; 5] = inPairs [1; 2; 3; 4]
let t3c = inPairs [1] = inPairs []
let t3d = inPairs [1; 2; 3; 4; 5; 6] = inPairs [1; 2; 3; 4; 5; 6; 7]
let t3e = inPairs [] = []

let t4a = flatten [[1; 2; 3]; []; [4; 5]; [6]] = [1; 2; 3; 4; 5; 6]
let t4b = flatten [[1; 2; 3]; [1; 2; 3]; [1; 2; 3]; [1; 2; 3]] 
        = [1; 2; 3; 1; 2; 3; 1; 2; 3; 1; 2; 3]
let t4c = flatten [[]; []; []; []; []; []; []] = flatten []
let t4d = flatten [[]] = flatten []
let t4e = flatten [] = []
let t4f = flatten [[3; 3; 3]; []; [3; 3]; [3]] = [3; 3; 3; 3; 3; 3]
let t4g = flatten [[1]; []; [2]; []; [3]; []; [4; 5]] = [1; 2; 3; 4; 5]
let t4h = flatten [[5]; []; []; []; []; []; []] = flatten [[5]]
let t4i = flatten [[]] = flatten []
let t4j = flatten [] = []


let t5a = remove (3, [3; 4; 3; 1]) = [4; 1];;
let t5b = remove (3, [3; 4; 3; 3; 3; 3; 3; 3; 3; 3; 3; 1]) = remove(3, [4; 1])
let t5c = let rec removeMultiple ((x,y) : int list * int list) =
                  match x with
                  | []      -> raise (Failure "removeMultiple")
                  | t :: [] -> remove (t, y)
                  | t :: rest -> removeMultiple (rest, remove (t, y))
          in removeMultiple ([3; 5; 6], [3; 6; 5]) = [];;
let t5c = let rec removeMultiple ((x,y) : int list * int list) =
                  match x with
                  | []      -> raise (Failure "removeMultiple")
                  | t :: [] -> remove (t, y)
              | t :: rest -> removeMultiple (rest, remove (t, y))
          in removeMultiple ([3; 5], [6; 5; 3; 3; 6; 6; 5; 3; 6; 6])
             = removeMultiple ([3; 5], [6; 6; 6; 6; 6])


let t6a = removeDups [4; 1; 2; 1; 4; 5; 20] = [4; 1; 2; 5; 20]
let t6b = removeDups [4; 1; 1; 2; 1; 1; 1; 2; 1; 4; 5; 20; 2; 1; 4; 5; 20]
        = removeDups [4; 1; 2; 1; 4; 5; 20]
let t6c = removeDups [] = []    
let t6d = (let x = [4; 1; 2; 5; 20] in (let y = x@x@x@x@x@x@x@x
                                      in (removeDups (y) = x) && (y != x)))

let t7a = collateSome [Some 1; None; Some 2; Some 1; None; Some 3] = [1; 2; 1; 3]
let t7b = collateSome [None; None; None; None; None; None] = []
let t7c = collateSome [None; None; Some 1; None; None; None; None] = [1]
let t7d = let x = [Some 1; None; Some 2; Some 1; None; Some 3]
           in removeDups(collateSome(x)) = [1; 2; 3] 
let t7e = removeDups (collateSome [None; None; None; None; None; None]) = []
let t7f = collateSome [None; None; Some 1; None; None; None; Some 1] = [1; 1]

let t8a = unzip2 [(1, 2); (3, 4); (5, 6)] = ([1; 3; 5], [2; 4; 6])
let t8b = unzip2 [] = ([], []);;
let t8c = unzip2 [(1, 3); (4, 5)] = ([1;4], [3; 5])

let t9a = makeChange (20, [8; 3; 2]) = Some [8; 8; 2; 2]
let t9b = makeChange (20, [8; 3]) = Some [8; 3; 3; 3; 3]
let t9c = makeChange (20, [13; 11]) = None
let t9d = makeChange (0, []) = None
let t9e = makeChange (20, []) = None
let t9f = makeChange (37, []) = None
let t9g = makeChange (0, [1]) = None
let t9a = makeChange (20, [8; 3; 2]) = Some [8; 8; 2; 2]
let t9b = makeChange (20, [8; 3]) = Some [8; 3; 3; 3; 3]
let t9c = makeChange (20, [13; 11]) = None
let t9d = makeChange (0, []) = None
let t9e = makeChange (20, []) = None
let t9f = makeChange (37, []) = None
let t9g = makeChange (0, [1]) = None
let t9h = makeChange (138, [9; 6; 3; 2; 1])
        = Some [9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 3]
        (* not Some [9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 6; 6]
           nor Some [9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 6; 3; 3]
       *);;
let t9h = makeChange (137, [9; 6; 3; 2; 1])
        = Some [9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 2]
let t9h = makeChange (136, [9; 6; 3; 2])
        = Some [9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 6; 2; 2]