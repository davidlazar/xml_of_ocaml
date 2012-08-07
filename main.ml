(* Executable frontend to the OCaml->XML pretty-printer
 * Author: David Lazar
 *)
open Camlp4.PreCast

module Caml = Camlp4OCamlParser.Make(Camlp4OCamlRevisedParser.Make(Syntax))

let parse f =
    let ic = open_in f in
    let strm = Stream.of_channel ic in
    let res = Caml.parse_implem (Loc.mk f) strm in
    close_in ic; res

let xml_of_file f =
    ASTToXML.print_str_item Format.str_formatter (parse f);
    Format.flush_str_formatter ()

let main () =
    let argc = Array.length Sys.argv in
    if argc < 2 then
        (Format.printf "Usage: %s <FILE>\n" Sys.executable_name; exit 1)
    else
        let xml = xml_of_file Sys.argv.(1) in
        print_endline xml
;;

try main ()
with e ->
    Format.eprintf "error: %a@." Camlp4.ErrorHandler.print e;
    exit 1
