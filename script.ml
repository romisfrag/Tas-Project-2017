open Js_of_ocaml


open Js_of_ocaml
module Html = Dom_html
       
     
let onload _ =
  let d = Html.document in
  let body =
    Js.Opt.get (d##getElementById (Js.string "wiki_demo"))
      (fun () -> assert false) in
  let textbox = Html.createTextarea d in
  ()
