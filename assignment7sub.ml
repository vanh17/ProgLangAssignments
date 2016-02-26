(* Programming Languages, Assignment 7 *)
(*
   You should write your functions in this file.
   You should NOT specify the types of your functions. Let the system determine
   them for you.

   The instructions for this assignment reside in an auxiliary file, assignment7doc.md
   You should start by reading that file.
*)
(* ---------------------------------
              HELPERS
   ---------------------------------

   Place your "helpers" implementations here.
*)
let rec range a b = if a > b then [] else a :: range (a + 1) b
(* Write a function `range1` that takes as input a single integer as input and returns 
the list of integers from 1 to that integer. This should be extremely easy. Use `range`. 
It should have type `int -> int list`.*)
let range1 n = range 1 n

(* Write a function `tabulate` that takes as input a function of type `int -> 'a` and also an integer `n` 
and uses it to generate the `'a list` consisting of the values of the provided function on the integers `1` through `n`. 
Reference solution is one line. Should have type: `(int -> 'a) -> int -> 'a list`*)

(* This implementation is for the purpose of learning fold_right *)
(*let tabulate f n = List.fold_right (fun x rest -> f x :: rest) (range1 n) []*)
(*shorter version*)
let tabulate f n = List.map f (range1 n)


(* ---------------------------------
              PICTURES
   ---------------------------------

   Place our Pictures implementations here after the type declarations and
   sword definition.
*)
type pixel = D | H
type row = pixel list
type pic = row list

exception IncompatibleDims

let sword = [
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D];
[D;H;H;D;D;D;D;D;D;D;D;D;D;D;D;D];
[D;H;H;H;H;D;D;D;D;D;D;D;D;D;D;D];
[D;D;H;H;H;H;D;D;D;D;D;D;D;D;D;D];
[D;D;H;H;H;H;H;D;D;D;D;D;D;D;D;D];
[D;D;D;H;H;H;H;D;D;D;D;D;D;D;D;D];
[D;D;D;D;H;H;H;H;D;D;D;D;D;D;D;D];
[D;D;D;D;D;D;H;H;H;D;D;D;D;D;D;D];
[D;D;D;D;D;D;D;H;H;H;D;D;H;D;D;D];
[D;D;D;D;D;D;D;D;H;H;D;H;H;D;D;D];
[D;D;D;D;D;D;D;D;D;D;H;H;D;D;D;D];
[D;D;D;D;D;D;D;D;D;H;H;H;D;D;D;D];
[D;D;D;D;D;D;D;D;H;H;D;D;H;D;D;D];
[D;D;D;D;D;D;D;D;D;D;D;D;D;H;D;D]]

(*
   You need to fix this.
*)
(* Create your own "picture", named `doodad`, in a manner similar to "sword" above. The picture can be anything you like, but it must satisfy the following conditions which will be checked by the grading tests:
    - It must be bound to the variable `doodad`.
    - It must be of type "pic".
    - It must be "rectangular", in other words all rows must have the same number of elements.
    - The two "dimensions" (number of rows, number of columns) do not have to be equal but they have to each be at least 6. (no 4x2 doodads)

    Should have type: `pic`*)
let doodad = [
[D;D;H;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D];
[D;H;H;H;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H;H;D;D];
[H;H;H;H;H;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H;H;H;H;H;H;H;H;D;D;D;D;D;D;D;D;D;D;H;H;H;H;H;D];
[H;H;H;H;H;H;D;D;D;D;D;D;D;D;D;D;D;H;H;H;H;H;H;H;H;H;H;D;D;D;D;D;D;D;D;D;D;H;H;H;H;H;D];
[H;H;H;H;H;H;D;D;D;D;D;D;D;D;D;D;H;H;H;H;H;H;H;H;H;H;H;D;D;D;D;D;D;D;D;D;D;D;H;H;H;D;D];
[D;D;H;H;D;D;D;D;D;D;D;D;D;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D];
[D;D;H;H;D;D;D;D;D;D;D;D;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D];
[D;D;H;H;D;D;D;D;D;D;D;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D];
[D;D;H;H;D;D;D;D;D;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;D;D;D;D;D;D;D;D;D;D;D;D];
[D;D;H;H;D;D;D;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;D;D;D;D;D;D;D;D];
[D;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;D;D;D;D];
[H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H;H];
[D;D;D;D;D;H;H;H;H;H;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H;H;H;H;D;D;D;D;D;D;D;D;D];
[D;D;D;D;D;H;H;H;H;H;H;H;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H;H;H;H;H;D;D;D;D;D;D;D;D];
[D;D;D;D;D;H;H;H;H;H;H;H;H;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H;H;H;H;H;H;D;D;D;D;D;D;D];
[D;D;D;D;D;H;H;H;H;H;H;H;H;H;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H;H;H;H;H;H;H;D;D;D;D;D;D];
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D];
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D];
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D];
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H;D;D;D;D;D;H;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D];
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H;H;H;H;H;H;H;H;H;H;H;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D];
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H;H;H;H;H;H;H;H;H;H;H;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D];
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H;D;D;D;D;D;H;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D];
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D]]

(*
   These two functions provided to you. Study how they work before continuing!
*)
let valid_pic pic =
   match List.map List.length pic with
   | [] -> true
   | x :: xs -> List.for_all ((=) x) xs

let dims_pic pic =
   match pic with
   | [] -> (0, 0)
   | row :: _ -> (List.length pic, List.length row)

(*
   Add your other functions here
*)

(*  Write a function `string_of_pxl` that takes as input a pixel and it returns a string,
 a single dot if the pixel is a D, the hash/pound sign # if the pixel is H. 
This is a very simple function. Should have type: `pixel -> string`
*)
let string_of_pxl pxl = if pxl = D then "." else "#"

(* 
Write a function `string_of_row` that takes as input a row and returns a string 
consisting of the concatenation of strings corresponding to the pixels in the row.  
The resulting string should include a newline `"\n"` at the end. 
Reference solution is 1 line. Should have type: `row -> string`
*)
let string_of_row row = List.fold_right (fun x acc -> (string_of_pxl x) ^ acc) row "\n"

(* 
Write a function `string_of_pic` that takes as input a picture and 
returns a string consisting of the concatenation of the strings produced by each row. 
The rows already contain newline characters. You do not need to add any extra newlines. 
Reference solution is 1 line. Should have type: `pic -> string`
*)
let string_of_pic pic = List.fold_right (fun x acc -> (string_of_row x) ^ acc) pic ""

(*
Write a function `flip_vertical` that takes as input a picture and 
returns a picture that is a vertical flip of the original one. 
Reference solution is 1 line.Should have type: `pic -> pic`
*)

(*This implementation is for the purpose of learning fold_left*)
(*let flip_vertical pic = List.fold_left (fun acc x -> x :: acc) [] pic*)
(***Shorter version***)
let flip_vertical pic = List.rev pic

(*
Write a function `flip_horizontal` that takes as input a picture and 
returns a picture that is a horizontal flip of the original one. 
Reference solution is 1 line. Should have type: `pic -> pic`
*)

(*This implementation is for the purpose of learning fold_left*)
(*let flip_horizontal pic = List.fold_right (fun x acc -> (List.fold_left (fun acc y -> y :: acc) [] x) :: acc) pic []*)
(***Shorter version***)
let flip_horizontal pic = List.map List.rev pic

(*
 Write a function `flip_both` that takes as input a picture and 
 returns a picture that is a "double flip" of the original one, 
 doing both a horizontal and a vertical flip. The order of flips would not matter. 
 Reference solution is 1 line. Should have type: `pic -> pic`
*)
let flip_both pic = flip_vertical (flip_horizontal pic)

(*
Write a function `mirror_vertical` that takes as input a picture and 
returns a picture that contains two copies of the original picture, 
one below the other but where the second copy is a mirror image of the first one, 
as produced by `flip_vertical`. You do not have to use `flip_vertical` along the way. 
It will have twice the number of rows as the original one. 
Reference solution is 1 line. Should have type: `pic -> pic`
*)

(*This implementation is for the purpose of learning fold_right*)
(*let mirror_vertical pic = List.fold_right (fun x acc -> x :: acc) pic (flip_vertical pic) *) 
(***Shorter version***)
let mirror_vertical pic = pic @ List.rev pic

(*
Write a function `mirror_horizontal` that takes as input a picture and 
returns a picture that contains two copies of the original picture, 
one next the other horizontally, but where the second copy is a mirror 
image of the first one as produced for example by `flip_horizontal` 
(but you will likely not be able to call `flip_horizontal`). 
It will have twice as long rows as the original one. 
Reference solution is 1 line. Should have type: `pic -> pic`
*)

(*This is the implmentation for purpose of learning List.fold_right*)
(*let mirror_horizontal pic = List.fold_right (fun x acc -> (x @ List.rev x) :: acc) pic []*)
(***Shorter version***)
let mirror_horizontal pic = List.map (fun x -> x @ List.rev x) pic

(* 
Write a function `mirror_both` that takes as input a picture and 
returns a picture that contains four copies of the original picture, 
arranged in a 2x2 format, and each obtained from the nearby one via
a flip in that direction. When applied to the sword picture, 
it should produce four swords emanating from its center. 
Reference solution is 1 line. Should have type: `pic -> pic`
*)
let mirror_both pic = mirror_vertical (mirror_horizontal pic)

(*
Write a function `pixelate` that takes as input a function 
`f` of type `int -> int -> pixel` and two integers `m` and `n`, in that order, 
and produces a picture of `m` rows and `n` "columns". 
The pixel at the i-th row and j-th column (in other words the j-th pixel in the i-th row) 
is determined by the value of the function `f`. Use `tabulate` from earlier (in two places). 
Reference solution is 1-2 lines. Should have type: `(int -> int -> pixel) -> int -> int -> pic`
*)
let pixelate f m n = tabulate (fun x -> tabulate (f x) n) m

(*
Write a function `stack_vertical` that takes as input two pictures and 
places them one atop the other in a vertical fashion. It should raise 
the exception `IncompatibleDims` if the pictures have different number of columns. 
Reference solution is 2 lines. Should have type: `pic -> pic -> pic`.
*)

(*let stack_vertical pic1 pic2 = let ((pic1r, pic1c), (pic2r, pic2c)) = (dims_pic pic1, dims_pic pic2) 
                               in if pic1c = pic2c then List.fold_right (fun x acc -> x :: acc) pic1 pic2 
                                  else raise (IncompatibleDims)*) 
(* this implementation is for purpose of learning more about fold_right*)
(***Shorter version***)
let stack_vertical pic1 pic2 = let ((pic1r, pic1c), (pic2r, pic2c)) = (dims_pic pic1, dims_pic pic2) 
                               in if pic1c = pic2c then pic1 @ pic2 
                                  else raise (IncompatibleDims)

(*
Write a function `stack_horizontal` that takes as input two pictures and 
places them next to each other in a horizontal fashion. It should 
raise the exception `IncompatibleDims` if the pictures have different number of rows. 
You should use `List.fold_right2`. You can handle the dimension check by either using the 
function `dims` provided earlier or letting `List.fold_right2` 
throw its exception (see documentation) and catching it. Or try them both! 
Reference solution is 2-4 lines. Should have type: `pic -> pic -> pic`.
*)
let stack_horizontal pic1 pic2 = if List.length pic1 = List.length pic2 then List.fold_right2 (fun x y acc -> (x @ y) :: acc) pic1 pic2 []
                                 else raise (IncompatibleDims)

(*
Write a function `invert` that takes as input a picture and returns the same picture with the two "colors" inverted. 
Reference solution is 2-4 lines. Should have type: `pic -> pic`
*)

(*This implementation is for the purpose of learning List.fold_right*)
(*let invert pic = List.fold_right (fun x acc -> (List.fold_right (fun x1 acc1 -> if x1 = D then H :: acc1 else D :: acc1) x []) :: acc) pic []*)
(***Shorter version***)
let invert pic = List.map (fun x -> List.map (fun y -> if y = D then H else D) x) pic

(*
Write a function `transpose` that takes as input a picture and returns the result of "transposing" the picture, i.e. 
turning its rows into columns. This one is short but tricky. The reference solution is 4 lines and 
uses `List.fold_right`, `List.map` and `List.map2` along with a let binding and a conditional. 
Start by working out manually in a small example how a recursive implementation might function 
(but your final solution is not meant to be recursive; this would just help you figure out the kind of work that your `fold_right` would have to do). 
Should have type: `pic -> pic`
*)
(*let transpose pic = match pic with
                    | [] -> []
                    | row :: rest -> List.fold_left (fun acc x-> List.map2 (fun t1 t2 -> t1 @ t2) acc (List.map (fun x2 -> x2 :: []) x)) 
                                     (List.map (fun x3 -> x3 :: []) row) rest *)
let transpose pic = match pic with
                    | [] -> []
                    | row :: rest -> List.fold_right (fun x acc -> List.map2 (fun t1 t2 -> t1 @ t2) acc (List.map (fun x2 -> x2 :: []) x)) 
                                     (List.rev rest) (List.map (fun x3 -> x3 :: []) row)