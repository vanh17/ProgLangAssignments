let t1a = range1 0 = []
let t1b = range1 1 = [1]
let t1c = range1 2 = [1; 2]
let t1d = range1 5 = [1; 2; 3; 4; 5]
let t1e = range1 10 = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
let t1f = range1 (-2) = []


let t2a = tabulate (fun x -> x * x) 5 = [1; 4; 9; 16; 25]
let t2b = tabulate (fun x -> true) 2 = [true; true]
let t2c = tabulate (fun x -> ()) 0 = []
let t2d = tabulate (fun x -> tabulate (fun y -> y * y) x) 4 = [[1]; [1; 4]; [1; 4; 9]; [1; 4; 9; 16]]
let t2e = tabulate (fun x-> []) (-2) =[]


let t3a = valid_pic doodad
let t3b = dims_pic doodad = (24, 43)

let t4a = string_of_pxl D = "."
let t4b = string_of_pxl H ="#"

