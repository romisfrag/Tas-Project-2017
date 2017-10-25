open Js_of_ocaml
open Anex

let add x y = x + y
let lol x y = x + y

let xd x y = x + y

let mdr x y = x + y

let ptdr x y = x + y

let xdddd x y = x + y


let _ =
  Js.export "add_o"
            (object%js
               method plus x y = add x y
               method abs x = fun_annexe x
               val zero = mdr 0 0
             end)
