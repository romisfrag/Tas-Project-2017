open Js_of_ocaml
open Types
open TypeChecker
       
module Html = Dom_html

let _ =
  Js.export_all
    (object%js
       method jbind t f = bind t f
       val zero = 0.
     end)

