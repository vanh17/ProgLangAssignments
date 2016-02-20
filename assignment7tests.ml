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

let t5a = string_of_row [D;D;H;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;
                         D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D] 
        = "..#........................................\n"
let t5b = string_of_row [D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D] = "................\n"
let t5c = string_of_row [D;D;H;H;D;D;D;D;D;H;H;H;H;H;H;H;H;H;H;H;H;H;H;
                         H;H;H;H;H;H;H;H;D;D;D;D;D;D;D;D;D;D;D;D] 
        = "..##.....######################............\n"
let t5d = string_of_row [D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H;D;D;D;D;D;
                        D;H;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D]
        = "...............##......#...................\n"
let t5e = string_of_row [] = "\n"

let t6a = string_of_pic [[D;D;H;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;
                          D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D];
                         [D;H;H;H;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;
                          D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H;H;D;D]]
        = "..#........................................\n.###..................................###..\n"
let t6b = string_of_pic [] = ""
let t6c = string_of_pic [[D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H;D;D;D;D;D;
                        D;H;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D]]
        = "...............##......#...................\n"
let t6d = string_of_pic doodad = "..#........................................\n" ^
".###..................................###..\n" ^
"#####.............#########..........#####.\n" ^
"######...........##########..........#####.\n" ^
"######..........###########...........###..\n" ^
"..##.........###############...............\n" ^
"..##........################...............\n" ^
"..##.......#################...............\n" ^
"..##.....######################............\n" ^
"..##...############################........\n" ^
".######################################....\n" ^
"###########################################\n" ^
".....#####...................#####.........\n" ^
".....#######.................######........\n" ^
".....########................#######.......\n" ^
".....#########...............########......\n" ^
"...........................................\n" ^
"...........................................\n" ^
".....................#.....................\n" ^
"...............##.....#....................\n" ^
"...............############................\n" ^
"...............############................\n" ^
"...............##.....#....................\n" ^
".....................#.....................\n"

let t7a = flip_vertical [[D; D; D; D; D; D; D; D; D; D; D; D; D]; 
                         [H; H; H; H; H; H; H; H; H; H; H; H; H]]
        =  [[H; H; H; H; H; H; H; H; H; H; H; H; H];
            [D; D; D; D; D; D; D; D; D; D; D; D; D]]
let t7b = flip_vertical [] = []
let t7c = flip_vertical [[D; D; D; D; D; D; D; D; D; D; D; D; D]; 
                         [H; H; H; H; H; H; H; H; H; H; H; H; H];
                         [H; H; H; H; H; D; H; H; H; H; H; H; H]]
        = [[H; H; H; H; H; D; H; H; H; H; H; H; H]; 
           [H; H; H; H; H; H; H; H; H; H; H; H; H];
           [D; D; D; D; D; D; D; D; D; D; D; D; D]]
let t7d = flip_vertical [[H; H; H; H; H; D; H; H; H; H; H; H; H]; 
                         [H; H; H; H; H; H; H; H; H; H; H; H; H];
                         [D; D; D; D; D; D; D; D; D; D; D; D; D];
                         [H; H; H; H; H; H; H; H; H; H; H; H; H]]
        = [[H; H; H; H; H; H; H; H; H; H; H; H; H];
           [D; D; D; D; D; D; D; D; D; D; D; D; D];
           [H; H; H; H; H; H; H; H; H; H; H; H; H];
           [H; H; H; H; H; D; H; H; H; H; H; H; H]]

let t8a = flip_horizontal [[D; D; D; D; D; D; D; D; D; D; D; D; D]; 
                           [H; H; H; H; H; H; H; H; H; H; H; H; H]]
        =  [[D; D; D; D; D; D; D; D; D; D; D; D; D]; 
            [H; H; H; H; H; H; H; H; H; H; H; H; H]]
let t8b = flip_horizontal [] = []
let t8c = flip_horizontal [[H; H; H; H; D; D; D; D; D; D; D; D; D]; 
                           [D; H; H; H; H; D; D; H; H; H; H; H; H];
                           [H; H; H; H; H; D; H; H; H; D; D; D; D]]
        = [[D; D; D; D; D; D; D; D; D; H; H; H; H]; 
           [H; H; H; H; H; H; D; D; H; H; H; H; D];
           [D; D; D; D; H; H; H; D; H; H; H; H; H]]


let t9a = flip_both [[D; D; D; D; D; D; D; D; D; D; D; D; D]; 
                     [H; H; H; H; H; H; H; H; H; H; H; H; H]]
        =  [[H; H; H; H; H; H; H; H; H; H; H; H; H];
            [D; D; D; D; D; D; D; D; D; D; D; D; D]]
let t8b = flip_both [] = []
let t8c = flip_both [[H; H; H; H; D; D; D; D; D; D; D; D; D]; 
                     [D; H; H; H; H; D; D; H; H; H; H; H; H];
                     [H; H; H; H; H; D; H; H; H; D; D; D; D]]
        = [[D; D; D; D; H; H; H; D; H; H; H; H; H]; 
           [H; H; H; H; H; H; D; D; H; H; H; H; D];
           [D; D; D; D; D; D; D; D; D; H; H; H; H]]

let t10a = mirror_vertical [[D; D; D; D; D; D; D; D; D; D; D; D; D]; 
                            [H; H; H; H; H; H; H; H; H; H; H; H; H]]
        =  [[D; D; D; D; D; D; D; D; D; D; D; D; D]; 
            [H; H; H; H; H; H; H; H; H; H; H; H; H];
            [H; H; H; H; H; H; H; H; H; H; H; H; H];
            [D; D; D; D; D; D; D; D; D; D; D; D; D]]
let t10b = mirror_vertical [] = []
let t10c = mirror_vertical [[D; D; D; D; D; D; D; D; D; D; D; D; D]; 
                            [H; H; H; H; H; H; H; H; H; H; H; H; H];
                            [H; H; H; H; H; D; H; H; H; H; H; H; H]]
        = [[D; D; D; D; D; D; D; D; D; D; D; D; D]; 
           [H; H; H; H; H; H; H; H; H; H; H; H; H];
           [H; H; H; H; H; D; H; H; H; H; H; H; H];
           [H; H; H; H; H; D; H; H; H; H; H; H; H]; 
           [H; H; H; H; H; H; H; H; H; H; H; H; H];
           [D; D; D; D; D; D; D; D; D; D; D; D; D]]
let t10d = mirror_vertical [[H; H; H; H; H; D; H; H; H; H; H; H; H]; 
                            [H; H; H; H; H; H; H; H; H; H; H; H; H];
                            [D; D; D; D; D; D; D; D; D; D; D; D; D];
                            [H; H; H; H; H; H; H; H; H; H; H; H; H]]
        = [[H; H; H; H; H; D; H; H; H; H; H; H; H]; 
           [H; H; H; H; H; H; H; H; H; H; H; H; H];
           [D; D; D; D; D; D; D; D; D; D; D; D; D];
           [H; H; H; H; H; H; H; H; H; H; H; H; H];
           [H; H; H; H; H; H; H; H; H; H; H; H; H];
           [D; D; D; D; D; D; D; D; D; D; D; D; D];
           [H; H; H; H; H; H; H; H; H; H; H; H; H];
           [H; H; H; H; H; D; H; H; H; H; H; H; H]]


let t11a = mirror_horizontal [[D; D; D; D; D; D; D; D; D; D; D; D; D]; 
                              [H; H; H; H; H; H; H; H; H; H; H; H; H]]
        =  [[D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D]; 
            [H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H]]
let t11b = mirror_horizontal [] = []
let t11c = mirror_horizontal [[H; H; H; H; D; D; D; D; D; D; D; D; D]; 
                              [D; H; H; H; H; D; D; H; H; H; H; H; H];
                              [H; H; H; H; H; D; H; H; H; D; D; D; D]]
        = [[H; H; H; H; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; H; H; H; H]; 
           [D; H; H; H; H; D; D; H; H; H; H; H; H; H; H; H; H; H; H; D; D; H; H; H; H; D];
           [H; H; H; H; H; D; H; H; H; D; D; D; D; D; D; D; D; H; H; H; D; H; H; H; H; H]]


let t12a = mirror_both [[D; D; D; D; D; D; D; D; D; D; D; D; D]; 
                        [H; H; H; H; H; H; H; H; H; H; H; H; H]]
        =  [[D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D]; 
            [H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H];
            [H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H; H];
            [D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D]]
let t12b = mirror_both [] = []
let t12c = mirror_both [[H; H; H; H; D; D; D; D; D; D; D; D; D]; 
                        [D; H; H; H; H; D; D; H; H; H; H; H; H];
                        [H; H; H; H; H; D; H; H; H; D; D; D; D]]
        = [[H; H; H; H; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; H; H; H; H]; 
           [D; H; H; H; H; D; D; H; H; H; H; H; H; H; H; H; H; H; H; D; D; H; H; H; H; D];
           [H; H; H; H; H; D; H; H; H; D; D; D; D; D; D; D; D; H; H; H; D; H; H; H; H; H];
           [H; H; H; H; H; D; H; H; H; D; D; D; D; D; D; D; D; H; H; H; D; H; H; H; H; H];
           [D; H; H; H; H; D; D; H; H; H; H; H; H; H; H; H; H; H; H; D; D; H; H; H; H; D];
           [H; H; H; H; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; D; H; H; H; H]]


let t13a = pixelate (fun x y -> if x * y mod 2 = 0 then D else H) 2 3
         = [[H; D; H];
            [D; D; D]]
let t13b = pixelate (fun x y -> if x * y mod 2 = 0 then D else H) 0 3 = []
let t13c = pixelate (fun x y -> if x * y mod 2 = 0 then D else H) 0 3 = []
let t13d = pixelate (fun x y -> if x * y mod 2 = 0 then D else H) 4 5
         = [[H; D; H; D; H];
            [D; D; D; D; D];
            [H; D; H; D; H];
            [D; D; D; D; D]]
let t13e = pixelate (fun x y -> if (x + y) mod 2 = 0 then D else H) 4 5
         = [[D; H; D; H; D];
            [H; D; H; D; H];
            [D; H; D; H; D];
            [H; D; H; D; H]]