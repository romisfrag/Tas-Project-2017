open Js_of_ocaml
open Anex

let add x y = x + y
let lol x y = x + y

let xd x y = x + y

let mdr x y = x + y

let ptdr x y = x + y

let xdddd x y = x + y

         
let affiche_interne s  =
  Js.string((Js.to_string s ) ^ "lolololololol")
                      

let _ =
  Js.export "add_o"
            (object%js
               method plus x y = add x y
               method abs x = fun_annexe x
               method affiche s = affiche_interne s
               val zero = mdr 0 0
             end)
