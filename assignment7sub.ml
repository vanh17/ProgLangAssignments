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
let tabulate f n = List.fold_right (fun x rest -> f x :: rest) (range1 n) []


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
[D;D;D;D;D;D;D;D;D;D;D;D;D;H;D;D];
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H];
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H]]

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
let string_of_row row = List.fold_right (fun x acc-> (string_of_pxl x) ^ acc) row "\n"

(* 
Write a function `string_of_pic` that takes as input a picture and 
returns a string consisting of the concatenation of the strings produced by each row. 
The rows already contain newline characters. You do not need to add any extra newlines. 
Reference solution is 1 line. Should have type: `pic -> string`
*)
let string_of_pic pic = List.fold_right (fun x acc -> (string_of_row x) ^ acc) pic ""