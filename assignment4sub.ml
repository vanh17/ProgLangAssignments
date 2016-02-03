(* Programming Languages, Assignment 4 *)
(*
   You should write your functions in this file.
   You should NOT specify the types of your functions. Let the system determine
   them for you. In the first set of problems this will result in types that
   the system may report to you different than the instructions, because of
   the type alias. You should make sure that the resulting types are equivalent
   to the requested ones.
   Write your code right below the corresponding comment describing the
   function you are asked to write.
*)

(* ----------------------------------------
                   THUNKS
   ---------------------------------------- *)

(*
   A "thunk" is an unevaluated expression. We can represent those as functions
   `unit -> 'a`. Such a function takes no input, but the body does not get
   evaluated until the function is called. For example we could define a thunk
   as the function `let th = fun () -> 3 + 4`. The expression `3 + 4` will only
   be evaluated if and when we call the thunk, like so: `th ()`.

   In the first part of this assignment, you will build functions that work with
   thunks. It is important that you make sure that expressions are not evaluated
   until they have to.

   Start by looking at the type definition of a thunk.
*)
type 'a thunk = unit -> 'a

(*
   Write a function `thunk` that takes as input a function of type `unit -> 'a`
   and returns the `'a thunk` from it. This is an incredibly simple function.
   It should have type: (unit -> 'a) -> 'a thunk
*)



(*
   Write a function `thunk_of_value` that takes as input a value of type `'a` and
   returns the thunk that if evaluated would produce that value. Again an incredibly
   simple function.
   Should have type: 'a -> 'a thunk
*)



(*
   Write a function `thunk_of_eval` that takes as input a pair of a function `'a -> 'b`
   and a value `'a` and returns the `'b thunk` that if called would apply the function
   to the value. This is also a very simple function. Make sure that the function
   is not applied until the thunk is evaluated.
   It should have type: ('a -> 'b) * 'a -> 'a thunk
*)




(*
   Write a function `try_thunk` that takes as input a `'a thunk` and returns a value of
   type `'a option` as follows: If the thunk evaluates to a value `v`, it returns `Some v`.
   If the thunk raises any kind of exception, it returns `None`. You will need to use
   a `try-with` construct, search online for examples. Note that the part that comes
   after the "with" is a pattern.
   It should have type: 'a thunk -> 'a option
*)



(*
   Write a function `thunk_of_pair` that takes as input a pair of thunks, and returns
   the thunk that if evaluated would produce the pair of values that those two thunks
   would give. Make sure that the two provided thunks are not evaluated until the
   returned thunk is called.
   It should have type: 'a thunk * 'b thunk -> ('a * 'b) thunk
*)



(*
   Write a function `thunk_map` that takes as input a pair of a `'a thunk` and a
   function `'a -> 'b`, and returns a `'b thunk` that if called would return the
   result of evaluating the function with input the value produced by evaluating
   the first thunk. Make sure that the provided thunk is not evaluated until
   the returned thunk is called.
   It should have type: 'a thunk * ('a -> 'b) -> 'b thunk
*)



(*
   Write a function `thunk_of_list` that takes as input a list of `'a thunk`s and
   returns a `'a list thunk` that when evaluated would in turn evaluate all the
   thunks in the list in their list order and return the resulting `'a list`.
   Make sure that no thunks on the list are evaluated until the returned thunk is
   called.
   It should have type: 'a thunk list -> 'a list thunk
*)

