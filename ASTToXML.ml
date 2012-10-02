(* Pretty-prints the AST of an OCaml program as XML.
 * Author: David Lazar
 *
 * Note: this file started off as ASTToXML.ml from the OCamlPDB project,
 * <https://github.com/nbros/OcamlPDB>. Most of the original file has been
 * rewritten to suit my needs.
 *)
open Camlp4
open PreCast
open Ast

(* escape XML chars *)
let escape_string str : string =
    let str = Str.global_replace (Str.regexp "&") "&amp;" str in
    let str = Str.global_replace (Str.regexp "<") "&lt;" str in
    let str = Str.global_replace (Str.regexp ">") "&gt;" str in
    let str = Str.global_replace (Str.regexp "'") "&apos;" str in
    let str = Str.global_replace (Str.regexp "\"") "&quot;" str in
    str

let pp = Format.fprintf

(* printers for basic types *)
let pp_int f i = pp f "<Builtin><Integer>%d</Integer></Builtin>" i
let pp_str f s = pp f "<Builtin><String>%s</String></Builtin>" (escape_string s)
let pp_float f d = pp f "<Builtin><Float>%f</Float></Builtin>" d

let pp_loc f loc = pp f "<LocInfo>%a%a</LocInfo>" pp_int (Loc.start_off loc) pp_int (Loc.stop_off loc)

(* TODO currently we don't print locations, but this should be configurable *)
let pp' loc f fmt = pp f fmt;;
(* let pp' loc f fmt = let afmt = "<Loc>%a" ^^ fmt ^^ "</Loc>" in pp f afmt pp_loc loc;; *)

(* miscellaneous printers *)
(* TODO move these *)
let print_rec_flag f = function
    | ReRecursive -> pp_str f "rec"
    | ReNil -> pp_str f "nil"
    | ReAnt str -> pp f "<Antiquotation>%s</Antiquotation>" (escape_string str)

let print_direction_flag f = function
    | DiTo -> pp_str f "to"
    | DiDownto -> pp_str f "downto"
    | DiAnt str -> pp f "<Antiquotation>%s</Antiquotation>" (escape_string str)

let print_mutable_flag f = function
    | MuMutable -> pp_str f "mutable"
    | MuNil -> pp_str f "nil"
    | MuAnt str -> pp f "<Antiquotation>%s</Antiquotation>" (escape_string str)

let print_private_flag f = function
    | PrPrivate -> pp_str f "private"
    | PrNil -> pp_str f "nil"
    | PrAnt str -> pp f "<Antiquotation>%s</Antiquotation>" (escape_string str)

let print_virtual_flag f = function
    | ViVirtual -> pp_str f "virtual"
    | ViNil -> pp_str f "nil"
    | ViAnt str -> pp f "<Antiquotation>%s</Antiquotation>" (escape_string str)

let print_override_flag f = function
    | OvOverride -> pp_str f "override"
    | OvNil -> pp_str f "nil"
    | OvAnt str -> pp f "<Antiquotation>%s</Antiquotation>" (escape_string str)

let print_row_var_flag f = function
    | RvRowVar -> pp_str f "row_var"
    | RvNil -> pp_str f "nil"
    | RvAnt str -> pp f "<Antiquotation>%s</Antiquotation>" (escape_string str)

let rec print_strings' f = function
    | LNil -> pp f ""
    | LCons(str, r) -> pp f "%a%a" pp_str str print_strings' r
    | LAnt(str) -> pp f "<Antiquotation>%s</Antiquotation>" (escape_string str)

let print_strings f = pp f "<List>%a</List>" print_strings'

(* The type of identifiers (including path like Foo(X).Bar.y) *)
let rec print_ident f = function
    (* i . i *) (** Access in module *)
    | IdAcc(loc, ident1, ident2) -> pp' loc f "<IdAcc>%a%a</IdAcc>" print_ident ident1 print_ident ident2
    (* i i *) (** Application *)
    | IdApp(loc, ident1, ident2) -> pp' loc f "<IdApp>%a%a</IdApp>" print_ident ident1 print_ident ident2
    (* foo *) (** Lowercase identifier *)
    | IdLid(loc, name) -> pp' loc f "<IdLid>%a</IdLid>" pp_str name
    (* Bar *) (** Uppercase identifier *)
    | IdUid(loc, name) -> pp' loc f "<IdUid>%a</IdUid>" pp_str name
    (* $s$ *) (** Antiquotation *)
    | IdAnt(loc, name) -> pp' loc f "<IdAnt>%a</IdAnt>" pp_str name

(* Representation of types *)
and print_ctyp f = function
    (** Empty type *)
    | TyNil(loc) -> pp' loc f "<TyNil/>"
    (* t as t *) (* list 'a as 'a *) (** Type aliasing *)
    | TyAli(loc, ctyp1, ctyp2) -> pp' loc f "<TyAli>%a%a</TyAli>" print_ctyp ctyp1 print_ctyp ctyp2
    (* _ *) (** Wildcard *)
    | TyAny(loc) -> pp' loc f "<TyAny/>"
    (* t t *) (* list 'a *) (** Application *)
    | TyApp(loc, ctyp1, ctyp2) -> pp' loc f "<TyApp>%a%a</TyApp>" print_ctyp ctyp1 print_ctyp ctyp2
    (* t) -> t *) (* int) -> string *) (** Arrow *)
    | TyArr(loc, ctyp1, ctyp2) -> pp' loc f "<TyArr>%a%a</TyArr>" print_ctyp ctyp1 print_ctyp ctyp2
    (* #i *) (* #point *) (** Class type *)
    | TyCls(loc, ident) -> pp' loc f "<TyCls>%a</TyCls>" print_ident ident
    (* ~s:t *) (** Label type *)
    | TyLab(loc, name, ctyp) -> pp' loc f "<TyLab>%a%a</TyLab>" pp_str name print_ctyp ctyp
    (* i *) (* Lazy.t *) (** Type identifier *)
    | TyId(loc, ident) -> pp' loc f "<TyId>%a</TyId>" print_ident ident
    (* t == t *) (* type t = [ A | B ] == Foo.t *) (** Type manifest *)
    | TyMan(loc, ctyp1, ctyp2) -> pp' loc f "<TyMan>%a%a</TyMan>" print_ctyp ctyp1 print_ctyp ctyp2
    (* type t 'a 'b 'c = t constraint t = t constraint t = t *) (** Type declaration *)
    | TyDcl(loc, name, ctyps, ctyp, (*TODO*) constraints) -> pp' loc f "<TyDcl>%a%a%a%a</TyDcl>" pp_str name print_ctyps ctyps print_ctyp ctyp print_constraints constraints
    (* < (t)? (..)? > *) (* < move : int) -> 'a .. > as 'a  *) (**   Object type *)
    | TyObj(loc, ctyp, row_var_flag) -> pp' loc f "<TyObj>%a%a</TyObj>" print_ctyp ctyp print_row_var_flag row_var_flag
    (* ?s:t *) (** Optional label type *)
    | TyOlb(loc, name, ctyp) -> pp' loc f "<TyOlb>%a%a</TyOlb>" pp_str name print_ctyp ctyp
    (* ! t . t *) (* ! 'a . list 'a) -> 'a *) (** Polymorphic type *)
    | TyPol(loc, ctyp1, ctyp2) -> pp' loc f "<TyPol>%a%a</TyPol>" print_ctyp ctyp1 print_ctyp ctyp2
    (* 's *)
    | TyQuo(loc, name) -> pp' loc f "<TyQuo>%a</TyQuo>" pp_str name
    (* +'s *)
    | TyQuP(loc, name) -> pp' loc f "<TyQuP>%a</TyQuP>" pp_str name
    (* -'s *)
    | TyQuM(loc, name) -> pp' loc f "<TyQuM>%a</TyQuM>" pp_str name
    (* `s *) (** Polymorphic variant *)
    | TyVrn(loc, name) -> pp' loc f "<TyVrn>%a</TyVrn>" pp_str name
    (* { t } *) (* { foo : int ; bar : mutable string } *) (** Record *)
    | TyRec(loc, ctyp) -> pp' loc f "<TyRec>%a</TyRec>" print_ctyp ctyp
    (* t : t *) (** Field declaration *)
    | TyCol(loc, ctyp1, ctyp2) -> pp' loc f "<TyCol>%a%a</TyCol>" print_ctyp ctyp1 print_ctyp ctyp2
    (* t; t *) (** Semicolon-separated type list *)
    | TySem(loc, ctyp1, ctyp2) -> pp' loc f "<TySem>%a%a</TySem>" print_ctyp ctyp1 print_ctyp ctyp2
    (* t, t *) (** Comma-separated type list *)
    | TyCom(loc, ctyp1, ctyp2) -> pp' loc f "<TyCom>%a%a</TyCom>" print_ctyp ctyp1 print_ctyp ctyp2
    (* [ t ] *) (* [ A of int, string | B ] *) (** Sum type *)
    | TySum(loc, ctyp) -> pp' loc f "<TySum>%a</TySum>" print_ctyp ctyp
    (* t of t *) (* A of int *)
    | TyOf(loc, ctyp1, ctyp2) -> pp' loc f "<TyOf>%a%a</TyOf>" print_ctyp ctyp1 print_ctyp ctyp2
    (* t, t *)
    | TyAnd(loc, ctyp1, ctyp2) -> pp' loc f "<TyAnd>%a%a</TyAnd>" print_ctyp ctyp1 print_ctyp ctyp2
    (* t | t *) (** "Or" pattern between types *)
    | TyOr(loc, ctyp1, ctyp2) -> pp' loc f "<TyOr>%a%a</TyOr>" print_ctyp ctyp1 print_ctyp ctyp2
    (* private t *) (** Private type *)
    | TyPrv(loc, ctyp) -> pp' loc f "<TyPrv>%a</TyPrv>" print_ctyp ctyp
    (* mutable t *) (** Mutable type *)
    | TyMut(loc, ctyp) -> pp' loc f "<TyMut>%a</TyMut>" print_ctyp ctyp
    (* ( t ) *) (* (int * string) *) (** Tuple *)
    | TyTup(loc, ctyp) -> pp' loc f "<TyTup>%a</TyTup>" print_ctyp ctyp
    (* t * t *)
    | TySta(loc, ctyp1, ctyp2) -> pp' loc f "<TySta>%a%a</TySta>" print_ctyp ctyp1 print_ctyp ctyp2
    (* [ = t ] *)
    | TyVrnEq(loc, ctyp) -> pp' loc f "<TyVrnEq>%a</TyVrnEq>" print_ctyp ctyp
    (* [ > t ] *)
    | TyVrnSup(loc, ctyp) -> pp' loc f "<TyVrnSup>%a</TyVrnSup>" print_ctyp ctyp
    (* [ < t ] *)
    | TyVrnInf(loc, ctyp) -> pp' loc f "<TyVrnInf>%a</TyVrnInf>" print_ctyp ctyp
    (* [ < t > t ] *)
    | TyVrnInfSup(loc, ctyp1, ctyp2) -> pp' loc f "<TyVrnInfSup>%a%a</TyVrnInfSup>" print_ctyp ctyp1 print_ctyp ctyp2
    (* t & t *)
    | TyAmp(loc, ctyp1, ctyp2) -> pp' loc f "<TyAmp>%a%a</TyAmp>" print_ctyp ctyp1 print_ctyp ctyp2
    (* t of & t *)
    | TyOfAmp(loc, ctyp1, ctyp2) -> pp' loc f "<TyOfAmp>%a%a</TyOfAmp>" print_ctyp ctyp1 print_ctyp ctyp2
    (* (module S) *) (** Package type **)
    | TyPkg (loc, module_type) -> pp' loc f "<TyPkg>%a</TyPkg>" print_module_type module_type
    (* $s$ *) (** Antiquotation *)
    | TyAnt(loc, name) -> pp' loc f "<TyAnt>%a</TyAnt>" pp_str name

(* The type of patterns *)
and print_patt f = function
    (**   Empty pattern *)
    | PaNil(loc) -> pp' loc f "<PaNil/>"
    (* i *) (** Identifier *)
    | PaId(loc, ident) -> pp' loc f "<PaId>%a</PaId>" print_ident ident
    (* p as p *) (* (Node x y as n) *) (** Alias *)
    | PaAli(loc, patt1, patt2) -> pp' loc f "<PaAli>%a%a</PaAli>" print_patt patt1 print_patt patt2
    (* $s$ *) (** Antiquotation *)
    | PaAnt(loc, name) -> pp' loc f "<PaAnt>%a</PaAnt>" pp_str name
    (* _ *) (** Wildcard *)
    | PaAny(loc) -> pp' loc f "<PaAny/>"
    (* p p *) (* fun x y) -> *) (** Application *)
    | PaApp(loc, patt1, patt2) -> pp' loc f "<PaApp>%a%a</PaApp>" print_patt patt1 print_patt patt2
    (* [| p |] *) (** Array *)
    | PaArr(loc, patt) -> pp' loc f "<PaArr>%a</PaArr>" print_patt patt
    (* p, p *) (** Comma-separated pattern list *)
    | PaCom(loc, patt1, patt2) -> pp' loc f "<PaCom>%a%a</PaCom>" print_patt patt1 print_patt patt2
    (* p; p *) (** Semicolon-separated pattern list *)
    | PaSem(loc, patt1, patt2) -> pp' loc f "<PaSem>%a%a</PaSem>" print_patt patt1 print_patt patt2
    (* c *) (* 'x' *) (** Character *)
    | PaChr(loc, name) -> pp' loc f "<PaChr>%a</PaChr>" pp_str name
    (** Integer *)
    | PaInt(loc, name) -> pp' loc f "<PaInt>%a</PaInt>" pp_int (int_of_string name)
    (** Int32 *)
    | PaInt32(loc, name) -> pp' loc f "<PaInt32>%a</PaInt32>" pp_int (int_of_string name)
    (** Int64 *)
    | PaInt64(loc, name) -> pp' loc f "<PaInt64>%a</PaInt64>" pp_int (int_of_string name)
    (** NativeInt *)
    | PaNativeInt(loc, name) -> pp' loc f "<PaNativeInt>%a</PaNativeInt>" pp_int (int_of_string name)
    (** Float *)
    | PaFlo(loc, name) -> pp' loc f "<PaFlo>%a</PaFlo>" pp_float (float_of_string name)
    (* ~s or ~s:(p) *) (** Label *)
    | PaLab(loc, name, patt) -> pp' loc f "<PaLab>%a%a</PaLab>" pp_str name print_patt patt
    (* ?s or ?s:(p) *) (** Optional label *)
    | PaOlb(loc, name, patt) -> pp' loc f "<PaOlb>%a%a</PaOlb>" pp_str name print_patt patt
    (* ?s:(p = e) or ?(p = e) *) (** Optional label with default value *)
    | PaOlbi(loc, name, patt, expr) -> pp' loc f "<PaOlbi>%a%a%a</PaOlbi>" pp_str name print_patt patt print_expr expr
    (* p | p *) (** Or *)
    | PaOrp(loc, patt1, patt2) -> pp' loc f "<PaOrp>%a%a</PaOrp>" print_patt patt1 print_patt patt2
    (* p .. p *) (** Pattern range *)
    | PaRng(loc, patt1, patt2) -> pp' loc f "<PaRng>%a%a</PaRng>" print_patt patt1 print_patt patt2
    (* { p } *) (** Record *)
    | PaRec(loc, patt) -> pp' loc f "<PaRec>%a</PaRec>" print_patt patt
    (* i = p *) (** Equality *)
    | PaEq(loc, ident, patt) -> pp' loc f "<PaEq>%a%a</PaEq>" print_ident ident print_patt patt
    (* s *) (** String *)
    | PaStr(loc, name) -> pp' loc f "<PaStr>%a</PaStr>" pp_str name
    (* ( p ) *) (** Tuple *)
    | PaTup(loc, patt) -> pp' loc f "<PaTup>%a</PaTup>" print_patt patt
    (* (p : t) *) (** Type constraint *)
    | PaTyc(loc, patt, ctyp) -> pp' loc f "<PaTyc>%a%a</PaTyc>" print_patt patt print_ctyp ctyp
    (* #i *)
    | PaTyp(loc, ident) -> pp' loc f "<PaTyp>%a</PaTyp>" print_ident ident
    (* `s *) (** Polymorphic variant *)
    | PaVrn(loc, name) -> pp' loc f "<PaVrn>%a</PaVrn>" pp_str name
    (* lazy p *)
    | PaLaz(loc, patt) -> pp' loc f "<PaLaz>%a</PaLaz>" print_patt patt
    (* (module M) (only on trunk) *)
    (*| PaMod (loc, name) -> pp' loc f "<PaMod>%a</PaMod>" pp_str name *)

(* The type of expressions *)
and print_expr f = function
    (** Empty expression *)
    | ExNil(loc) -> pp' loc f "<ExNil/>"
    (* i *) (**   Identifier *)
    | ExId(loc, ident) -> pp' loc f "<ExId>%a</ExId>" print_ident ident
    (* e.e *) (** Access in module *)
    | ExAcc(loc, expr1, expr2) -> pp' loc f "<ExAcc>%a%a</ExAcc>" print_expr expr1 print_expr expr2
    (* $s$ *) (** Antiquotation *)
    | ExAnt(loc, name) -> pp' loc f "<ExAnt>%a</ExAnt>" pp_str name
    (* e e *) (** Application *)
    | ExApp(loc, expr1, expr2) -> pp' loc f "<ExApp>%a%a</ExApp>" print_expr expr1 print_expr expr2
    (* e.(e) *) (** Array access *)
    | ExAre(loc, expr1, expr2) -> pp' loc f "<ExAre>%a%a</ExAre>" print_expr expr1 print_expr expr2
    (* [| e |] *) (** Array declaration *)
    | ExArr(loc, expr) -> pp' loc f "<ExArr>%a</ExArr>" print_expr expr
    (* e; e *) (** Semicolon-separated expression list *)
    | ExSem(loc, expr1, expr2) -> pp' loc f "<ExSem>%a%a</ExSem>" print_expr expr1 print_expr expr2
    (* assert False *) (** assert False *)
    | ExAsf(loc) -> pp' loc f "<ExAsf/>"
    (* assert e *) (** assert e *)
    | ExAsr(loc, expr) -> pp' loc f "<ExAsr>%a</ExAsr>" print_expr expr
    (* e := e *) (** Assignment *)
    | ExAss(loc, expr1, expr2) -> pp' loc f "<ExAss>%a%a</ExAss>" print_expr expr1 print_expr expr2
    (* 'c' *) (** Character *)
    | ExChr(loc, name) -> pp' loc f "<ExChr>%a</ExChr>" pp_str name
    (* (e : t) or (e : t :> t) *) (** Coercion *)
    | ExCoe(loc, expr, ctyp1, ctyp2) -> pp' loc f "<ExCoe>%a%a%a</ExCoe>" print_expr expr print_ctyp ctyp1 print_ctyp ctyp2
    (* 3.14 *) (** Float *)
    | ExFlo(loc, name) -> pp' loc f "<ExFlo>%a</ExFlo>" pp_str name
    (* for s = e to/downto e do { e } *) (** For loop *)
    | ExFor(loc, name, expr1, expr2, direction_flag, expr3) -> pp' loc f "<ExFor>%a%a%a%a%a</ExFor>" pp_str name print_expr expr1 print_expr expr2 print_direction_flag direction_flag print_expr expr3
    (* fun [ mc ] *) (** Function with match case *)
    | ExFun(loc, match_case) -> pp' loc f "<ExFun>%a</ExFun>" print_match_case match_case
    (* if e then e else e *) (** if/then/else *)
    | ExIfe(loc, expr1, expr2, expr3) -> pp' loc f "<ExIfe>%a%a%a</ExIfe>" print_expr expr1 print_expr expr2 print_expr expr3
    (* 42 *) (** Int *)
    | ExInt(loc, name) -> pp' loc f "<ExInt>%a</ExInt>" pp_int (int_of_string name)
    (** Int32 *)
    | ExInt32(loc, name) -> pp' loc f "<ExInt32>%a</ExInt32>" pp_int (int_of_string name)
    (** Int64 *)
    | ExInt64(loc, name) -> pp' loc f "<ExInt64>%a</ExInt64>" pp_int (int_of_string name)
    (** NativeInt *)
    | ExNativeInt(loc, name) -> pp' loc f "<ExNativeInt>%a</ExNativeInt>" pp_int (int_of_string name)
    (* ~s or ~s:e *) (** Label argument with/without expression *)
    | ExLab(loc, name, expr) -> pp' loc f "<ExLab>%a%a</ExLab>" pp_str name print_expr expr
    (* lazy e *) (** Lazy evaluation *)
    | ExLaz(loc, expr) -> pp' loc f "<ExLaz>%a</ExLaz>" print_expr expr
    (* let b in e or let rec b in e *) (** Let statement with/without recursion *)
    | ExLet(loc, ReNil, binding, expr) -> pp' loc f "<ExLet>%a%a</ExLet>" print_binding binding print_expr expr
    | ExLet(loc, ReRecursive, binding, expr) -> pp' loc f "<ExLetRec>%a%a</ExLetRec>" print_binding binding print_expr expr
    (* let module s = me in e *) (** "Let module in" construct *)
    | ExLmd(loc, name, module_expr, expr) -> pp' loc f "<ExLmd>%a%a%a</ExLmd>" pp_str name print_module_expr module_expr print_expr expr
    (* match e with [ mc ] *) (** Match case *)
    | ExMat(loc, expr, match_case) -> pp' loc f "<ExMat>%a%a</ExMat>" print_expr expr print_match_case match_case
    (* new i *) (** New object *)
    | ExNew(loc, ident) -> pp' loc f "<ExNew>%a</ExNew>" print_ident ident
    (* object ((p))? (cst)? end *) (** Object declaration *)
    | ExObj(loc, patt, class_str_item) -> pp' loc f "<ExObj>%a%a</ExObj>" print_patt patt print_class_str_item class_str_item
    (* ?s or ?s:e *) (** Optional label *)
    | ExOlb(loc, name, expr) -> pp' loc f "<ExOlb>%a%a</ExOlb>" pp_str name print_expr expr
    (* {< rb >} *) (** Overloading *)
    | ExOvr(loc, rec_binding) -> pp' loc f "<ExOvr>%a</ExOvr>" print_rec_binding rec_binding
    (* { rb } or { (e) with rb } *) (** Record *)
    | ExRec(loc, rec_binding, expr) -> pp' loc f "<ExRec>%a%a</ExRec>" print_rec_binding rec_binding print_expr expr
    (* do { e } *) (** Sequence with "do" statement *)
    | ExSeq(loc, expr) -> pp' loc f "<ExSeq>%a</ExSeq>" print_expr expr
    (* e#s *) (** Method call *)
    | ExSnd(loc, expr, name) -> pp' loc f "<ExSnd>%a%a</ExSnd>" print_expr expr pp_str name
    (* e.[e] *) (** String access *)
    | ExSte(loc, expr1, expr2) -> pp' loc f "<ExSte>%a%a</ExSte>" print_expr expr1 print_expr expr2
    (* s *) (* "foo" *) (** String *)
    | ExStr(loc, name) -> pp' loc f "<ExStr>%a</ExStr>" pp_str name
    (* try e with [ mc ] *) (** "Try .. with" construct *)
    | ExTry(loc, expr, match_case) -> pp' loc f "<ExTry>%a%a</ExTry>" print_expr expr print_match_case match_case
    (* (e) *) (** Tuple *)
    | ExTup(loc, expr) -> pp' loc f "<ExTup>%a</ExTup>" print_expr expr
    (* e, e *) (** Comma-separated expression list *)
    | ExCom(loc, expr1, expr2) -> pp' loc f "<ExCom>%a%a</ExCom>" print_expr expr1 print_expr expr2
    (* (e : t) *) (** Type constraint *)
    | ExTyc(loc, expr, ctyp) -> pp' loc f "<ExTyc>%a%a</ExTyc>" print_expr expr print_ctyp ctyp
    (* `s *) (** Polymorphic variant *)
    | ExVrn(loc, name) -> pp' loc f "<ExVrn>%a</ExVrn>" pp_str name
    (* while e do { e } *) (** "While .. do" constraint *)
    | ExWhi(loc, expr1, expr2) -> pp' loc f "<ExWhi>%a%a</ExWhi>" print_expr expr1 print_expr expr2
    (* let open i in e *) (** Local module opening *)
    | ExOpI (loc, ident, expr) -> pp' loc f "<ExOpI>%a%a</ExOpI>" print_ident ident print_expr expr
    (* fun (type t) -> e *)
    (* let f x (type t) y z = e *)
    | ExFUN (loc, name, expr) -> pp' loc f "<ExFUN>%a%a</ExFUN>" pp_str name print_expr expr
    (* (module ME : S) which is represented as (module (ME : S)) *) (** expression to pack a module as a first-class value *)
    | ExPkg (loc, module_expr) -> pp' loc f "<ExPkg>%a</ExPkg>" print_module_expr module_expr

(* The type of module types *)
and print_module_type f = function
    | MtNil(loc) -> pp' loc f "<MtNil/>"
    (* i *) (* A.B.C *)
    | MtId(loc, ident) -> pp' loc f "<MtId>%a</MtId>" print_ident ident
    (* functor (s : mt)) -> mt *)
    | MtFun(loc, name, module_type1, module_type2) -> pp' loc f "<MtFun>%a%a%a/MtFun>" pp_str name print_module_type module_type1 print_module_type module_type2
    (* 's *)
    | MtQuo(loc, name) -> pp' loc f "<MtQuo>%a</MtQuo>" pp_str name
    (* sig sg end *)
    | MtSig(loc, sig_item) -> pp' loc f "<MtSig>%a</MtSig>" print_sig_item sig_item
    (* mt with wc *)
    | MtWit(loc, module_type, with_constr) -> pp' loc f "<MtWit>%a%a</MtWit>" print_module_type module_type print_with_constr with_constr
    (* $s$ *)
    | MtAnt(loc, name) -> pp' loc f "<MtAnt>%a</MtAnt>" pp_str name

(* The type of signature items *)
and print_sig_item f = function
    | SgNil(loc) -> pp' loc f "<SgNil/>"
    (* class cict *)
    | SgCls(loc, class_type) -> pp' loc f "<SgCls>%a</SgCls>" print_class_type class_type
    (* class type cict *)
    | SgClt(loc, class_type) -> pp' loc f "<SgClt>%a</SgClt>" print_class_type class_type
    (* sg ; sg *)
    | SgSem(loc, sig_item1, sig_item2) -> pp' loc f "<SgSem>%a%a</SgSem>" print_sig_item sig_item1 print_sig_item sig_item2
    (* # s or # s e *)
    | SgDir(loc, name, expr) -> pp' loc f "<SgDir>%a%a</SgDir>" pp_str name print_expr expr
    (* exception t *)
    | SgExc(loc, ctyp) -> pp' loc f "<SgExc>%a</SgExc>" print_ctyp ctyp
    (* external s : t = s ... s *)
    | SgExt(loc, name, ctyp, strings (*meta_list string*)) -> pp' loc f "<SgExt>%a%a%a</SgExt>" pp_str name print_ctyp ctyp print_strings strings
    (* include mt *)
    | SgInc(loc, module_type) -> pp' loc f "<SgInc>%a</SgInc>" print_module_type module_type
    (* module s : mt *)
    | SgMod(loc, name, module_type) -> pp' loc f "<SgMod>%a%a</SgMod>" pp_str name print_module_type module_type
    (* module rec mb *)
    | SgRecMod(loc, module_binding) -> pp' loc f "<SgRecMod>%a</SgRecMod>" print_module_binding module_binding
    (* module type s = mt *)
    | SgMty(loc, name, module_type) -> pp' loc f "<SgMty>%a%a</SgMty>" pp_str name print_module_type module_type
    (* open i *)
    | SgOpn(loc, ident) -> pp' loc f "<SgOpn>%a</SgOpn>" print_ident ident
    (* type t *)
    | SgTyp(loc, ctyp) -> pp' loc f "<SgTyp>%a</SgTyp>" print_ctyp ctyp
    (* value s : t *)
    | SgVal(loc, name, ctyp) -> pp' loc f "<SgVal>%a%a</SgVal>" pp_str name print_ctyp ctyp
    (* $s$ *)
    | SgAnt(loc, name) -> pp' loc f "<SgAnt>%a</SgAnt>" pp_str name

(* The type of `with' constraints *)
and print_with_constr f = function
    | WcNil(loc) -> pp' loc f "<WcNil/>"
    (* type t = t *)
    | WcTyp(loc, ctyp1, ctyp2) -> pp' loc f "<WcTyp>%a%a</WcTyp>" print_ctyp ctyp1 print_ctyp ctyp2
    (* module i = i *)
    | WcMod(loc, ident1, ident2) -> pp' loc f "<WcMod>%a%a</WcMod>" print_ident ident1 print_ident ident2
    (* type t := t *)
    | WcTyS (loc, ctyp1, ctyp2) -> pp' loc f "<WcTyS>%a%a</WcTyS>" print_ctyp ctyp1 print_ctyp ctyp2
    (* module i := i *)
    | WcMoS (loc, ident1, ident2) -> pp' loc f "<WcMoS>%a%a</WcMoS>" print_ident ident1 print_ident ident2
    (* wc, wc *)
    | WcAnd(loc, with_constr1, with_constr2) -> pp' loc f "<WcAnd>%a%a</WcAnd>" print_with_constr with_constr1 print_with_constr with_constr2
    (* $s$ *)
    | WcAnt(loc, name) -> pp' loc f "<WcAnt>%a</WcAnt>" pp_str name

(* The type of let bindings *)
and print_binding f = function
    | BiNil(loc) -> pp' loc f "<BiNil/>"
    (* bi, bi *) (* let a = 42, print_c f = function 43 *)
    | BiAnd(loc, binding1, binding2) -> pp' loc f "<BiAnd>%a%a</BiAnd>" print_binding binding1 print_binding binding2
    (* p = e *) (* let patt = expr *)
    | BiEq(loc, patt, expr) -> pp' loc f "<BiEq>%a%a</BiEq>" print_patt patt print_expr expr
    (* $s$ *)
    | BiAnt(loc, name) -> pp' loc f "<BiAnt>%a</BiAnt>" pp_str name

(* The type of record definitions *)
and print_rec_binding f = function
    (** Empty record definition *)
    | RbNil(loc) -> pp' loc f "<RbNil/>"
    (* rb ; rb *)
    | RbSem(loc, rec_binding1, rec_binding2) -> pp' loc f "<RbSem>%a%a</RbSem>" print_rec_binding rec_binding1 print_rec_binding rec_binding2
    (* i = e *)
    | RbEq(loc, ident, expr) -> pp' loc f "<RbEq>%a%a</RbEq>" print_ident ident print_expr expr
    (* $s$ *)
    | RbAnt(loc, name) -> pp' loc f "<RbAnt>%a</RbAnt>" pp_str name

(* The type of recursive module definitions *)
and print_module_binding f = function
    (** Empty module definition *)
    | MbNil(loc) -> pp' loc f "<MbNil/>"
    (* mb, mb *) (* module rec (s : mt) = me, (s : mt) = me *)
    | MbAnd(loc, module_binding1, module_binding2) -> pp' loc f "<MbAnd>%a%a</MbAnd>" print_module_binding module_binding1 print_module_binding module_binding2
    (* s : mt = me *)
    | MbColEq(loc, name, module_type, module_expr) -> pp' loc f "<MbColEq>%a%a%a</MbColEq>" pp_str name print_module_type module_type print_module_expr module_expr
    (* s : mt *)
    | MbCol(loc, name, module_type) -> pp' loc f "<MbCol>%a%a</MbCol>" pp_str name print_module_type module_type
    (* $s$ *)
    | MbAnt(loc, name) -> pp' loc f "<MbAnt>%a</MbAnt>" pp_str name

(* The type of cases for match/function/try constructions     *)
and print_match_case f = function
    (** Empty case *)
    | McNil(loc) -> pp' loc f "<McNil/>"
    (* a | a *)
    | McOr(loc, match_case1, match_case2) -> pp' loc f "<McOr>%a%a</McOr>" print_match_case match_case1 print_match_case match_case2
    (* p (when e)?) -> e *)
    | McArr(loc, patt, expr1, expr2) -> pp' loc f "<McArr>%a%a%a</McArr>" print_patt patt print_expr expr1 print_expr expr2
    (* $s$ *)
    | McAnt(loc, name) -> pp' loc f "<McAnt>%a</McAnt>" pp_str name

(* The type of module expressions *)
and print_module_expr f = function
    (** Empty module expression *)
    | MeNil(loc) -> pp' loc f "<MeNil/>"
    (* i *)
    | MeId(loc, ident) -> pp' loc f "<MeId>%a</MeId>" print_ident ident
    (* me me *)
    | MeApp(loc, module_expr1, module_expr2) -> pp' loc f "<MeApp>%a%a</MeApp>" print_module_expr module_expr1 print_module_expr module_expr2
    (* functor (s : mt)) -> me *)
    | MeFun(loc, name, module_type, module_expr) -> pp' loc f "<MeFun>%a%a%a</MeFun>" pp_str name print_module_type module_type print_module_expr module_expr
    (* struct st end *)
    | MeStr(loc, str_item) -> pp' loc f "<MeStr>%a</MeStr>" print_str_item str_item
    (* (me : mt) *)
    | MeTyc(loc, module_expr, module_type) -> pp' loc f "<MeTyc>%a%a</MeTyc>" print_module_expr module_expr print_module_type module_type
    (* (value e) *)
    (* (value e : S) which is represented as (value (e : S)) *)
    | MePkg (loc, expr) -> pp' loc f "<MePkg>%a</MePkg>" print_expr expr
    (* $s$ *)
    | MeAnt(loc, name) -> pp' loc f "<MeAnt>%a</MeAnt>" pp_str name

(* The type of structure items *)
and print_str_item f = function
    | StNil(loc) -> pp' loc f "<StNil/>"
    (* class cice *)
    | StCls(loc, class_expr) -> pp' loc f "<StCls>%a</StCls>" print_class_expr class_expr
    (* class type cict *)
    | StClt(loc, class_type) -> pp' loc f "<StClt>%a</StClt>" print_class_type class_type
    (* st ; st *)
    | StSem(loc, str_item1, str_item2) -> pp' loc f "<StSem>%a%a</StSem>" print_str_item str_item1 print_str_item str_item2
    (* # s or # s e *)
    | StDir(loc, name, expr) -> pp' loc f "<StDir>%a%a</StDir>" pp_str name print_expr expr
    (* exception t or exception t = i *)
    | StExc(loc, ctyp, option_ident) -> pp' loc f "<StExc>%a%a</StExc>" print_ctyp ctyp print_option_ident option_ident
    (* e *)
    | StExp(loc, expr) -> pp' loc f "<StExp>%a</StExp>" print_expr expr
    (* external s : t = s ... s *)
    | StExt(loc, name, ctyp, (*TODO*) strings(*meta_list string*)) -> pp' loc f "<StExt>%a%a%a</StExt>" pp_str name print_ctyp ctyp print_strings strings
    (* include me *)
    | StInc(loc, module_expr) -> pp' loc f "<StInc>%a</StInc>" print_module_expr module_expr
    (* module s = me *)
    | StMod(loc, name, module_expr) -> pp' loc f "<StMod>%a%a</StMod>" pp_str name print_module_expr module_expr
    (* module rec mb *)
    | StRecMod(loc, module_binding) -> pp' loc f "<StRecMod>%a</StRecMod>" print_module_binding module_binding
    (* module type s = mt *)
    | StMty(loc, name, module_type) -> pp' loc f "<StMty>%a%a</StMty>" pp_str name print_module_type module_type
    (* open i *)
    | StOpn(loc, ident) -> pp' loc f "<StOpn>%a</StOpn>" print_ident ident
    (* type t *)
    | StTyp(loc, ctyp) -> pp' loc f "<StTyp>%a</StTyp>" print_ctyp ctyp
    (* value (rec)? bi *)
    | StVal(loc, rec_flag, binding) -> pp' loc f "<StVal>%a%a</StVal>" print_rec_flag rec_flag print_binding binding
    (* $s$ *)
    | StAnt(loc, name) -> pp' loc f "<StAnt>%a</StAnt>" pp_str name

(* The type of class types *)
and print_class_type f = function
    | CtNil(loc) -> pp' loc f "<CtNil/>"
    (* (virtual)? i ([ t ])? *)
    | CtCon(loc, virtual_flag, ident, ctyp) -> pp' loc f "<CtCon>%a%a%a</CtCon>" print_virtual_flag virtual_flag print_ident ident print_ctyp ctyp
    (* [t]) -> ct *)
    | CtFun(loc, ctyp, class_type) -> pp' loc f "<CtFun>%a%a</CtFun>" print_ctyp ctyp print_class_type class_type
    (* object ((t))? (csg)? end *)
    | CtSig(loc, ctyp, class_sig_item) -> pp' loc f "<CtSig>%a%a</CtSig>" print_ctyp ctyp print_class_sig_item class_sig_item
    (* ct, ct *)
    | CtAnd(loc, class_type1, class_type2) -> pp' loc f "<CtAnd>%a%a</CtAnd>" print_class_type class_type1 print_class_type class_type2
    (* ct : ct *)
    | CtCol(loc, class_type1, class_type2) -> pp' loc f "<CtCol>%a%a</CtCol>" print_class_type class_type1 print_class_type class_type2
    (* ct = ct *)
    | CtEq(loc, class_type1, class_type2) -> pp' loc f "<CtEq>%a%a</CtEq>" print_class_type class_type1 print_class_type class_type2
    (* $s$ *)
    | CtAnt(loc, name) -> pp' loc f "<CtAnt>%a</CtAnt>" pp_str name

(* The type of class signature items *)
and print_class_sig_item f = function
    | CgNil(loc) -> pp' loc f "<CgNil/>"
    (* type t = t *)
    | CgCtr(loc, ctyp1, ctyp2) -> pp' loc f "<CgCtr>%a%a</CgCtr>" print_ctyp ctyp1 print_ctyp ctyp2
    (* csg ; csg *)
    | CgSem(loc, class_sig_item1, class_sig_item2) -> pp' loc f "<CgSem>%a%a</CgSem>" print_class_sig_item class_sig_item1 print_class_sig_item class_sig_item2
    (* inherit ct *)
    | CgInh(loc, class_type) -> pp' loc f "<CgInh>%a</CgInh>" print_class_type class_type
    (* method s : t or method private s : t *)
    | CgMth(loc, name, private_flag, ctyp) -> pp' loc f "<CgMth>%a%a%a</CgMth>" pp_str name print_private_flag private_flag print_ctyp ctyp
    (* value (virtual)? (mutable)? s : t *)
    | CgVal(loc, name, mutable_flag, virtual_flag, ctyp) -> pp' loc f "<CgVal>%a%a%a%a</CgVal>" pp_str name print_mutable_flag mutable_flag print_virtual_flag virtual_flag print_ctyp ctyp
    (* method virtual (private)? s : t *)
    | CgVir(loc, name, private_flag, ctyp) -> pp' loc f "<CgVir>%a%a%a</CgVir>" pp_str name print_private_flag private_flag print_ctyp ctyp
    (* $s$ *)
    | CgAnt(loc, name) -> pp' loc f "<CgAnt>%a</CgAnt>" pp_str name

(* The type of class expressions *)
and print_class_expr f = function
    | CeNil(loc) -> pp' loc f "<CeNil/>"
    (* ce e *)
    | CeApp(loc, class_expr, expr) -> pp' loc f "<CeApp>%a%a</CeApp>" print_class_expr class_expr print_expr expr
    (* (virtual)? i ([ t ])? *)
    | CeCon(loc, virtual_flag, ident, ctyp) -> pp' loc f "<CeCon>%a%a%a</CeCon>" print_virtual_flag virtual_flag print_ident ident print_ctyp ctyp
    (* fun p -> ce *)
    | CeFun(loc, patt, class_expr) -> pp' loc f "<CeFun>%a%a</CeFun>" print_patt patt print_class_expr class_expr
    (* let (rec)? bi in ce *)
    | CeLet(loc, rec_flag, binding, class_expr) -> pp' loc f "<CeLet>%a%a%a</CeLet>" print_rec_flag rec_flag print_binding binding print_class_expr class_expr
    (* object ((p))? (cst)? end *)
    | CeStr(loc, patt, class_str_item) -> pp' loc f "<CeStr>%a%a</CeStr>" print_patt patt print_class_str_item class_str_item
    (* ce : ct *)
    | CeTyc(loc, class_expr, class_type) -> pp' loc f "<CeTyc>%a%a</CeTyc>" print_class_expr class_expr print_class_type class_type
    (* ce, ce *)
    | CeAnd(loc, class_expr1, class_expr2) -> pp' loc f "<CeAnd>%a%a</CeAnd>" print_class_expr class_expr1 print_class_expr class_expr2
    (* ce = ce *)
    | CeEq(loc, class_expr1, class_expr2) -> pp' loc f "<CeEq>%a%a</CeEq>" print_class_expr class_expr1 print_class_expr class_expr2
    (* $s$ *)
    | CeAnt(loc, name) -> pp' loc f "<CeAnt>%a</CeAnt>" pp_str name

(* The type of class structure items *)
and print_class_str_item f = function
    | CrNil(loc) -> pp' loc f "<CrNil/>"
    (* cst ; cst *)
    | CrSem(loc, class_str_item1, class_str_item2) -> pp' loc f "<CrSem>%a%a</CrSem>" print_class_str_item class_str_item1 print_class_str_item class_str_item2
    (* type t = t *)
    | CrCtr(loc, ctyp1, ctyp2) -> pp' loc f "<CrCtr>%a%a</CrCtr>" print_ctyp ctyp1 print_ctyp ctyp2
    (* inherit(!)? ce (as s)? *)
    | CrInh(loc, override_flag, class_expr, name) -> pp' loc f "<CrInh>%a%a%a</CrInh>" print_override_flag override_flag print_class_expr class_expr pp_str name
    (* initializer e *)
    | CrIni(loc, expr) -> pp' loc f "<CrIni>%a</CrIni>" print_expr expr
    (* method(!)? (private)? s : t = e or method(!)? (private)? s = e *)
    | CrMth(loc, name, override_flag, private_flag, expr, ctyp) -> pp' loc f "<CrMth>%a%a%a%a%a</CrMth>" pp_str name print_override_flag override_flag print_private_flag private_flag print_expr expr print_ctyp ctyp
    (* value(!)? (mutable)? s = e *)
    | CrVal(loc, name, override_flag, mutable_flag, expr) -> pp' loc f "<CrVal>%a%a%a%a</CrVal>" pp_str name print_override_flag override_flag print_mutable_flag mutable_flag print_expr expr
    (* method virtual (private)? s : t *)
    | CrVir(loc, name, private_flag, ctyp) -> pp' loc f "<CrVir>%a%a%a</CrVir>" pp_str name print_private_flag private_flag print_ctyp ctyp
    (* value virtual (mutable)? s : t *)
    | CrVvr(loc, name, mutable_flag, ctyp) -> pp' loc f "<CrVvr>%a%a%a</CrVvr>" pp_str name print_mutable_flag mutable_flag print_ctyp ctyp
    (* $s$ *)
    | CrAnt(loc, name) -> pp' loc f "<CrAnt>%a</CrAnt>" pp_str name

and print_list : 'a. (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
    = fun ppr f xs -> pp f "<List>%a</List>" (print_list' ppr) xs

and print_list' : 'a. (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
    = fun ppr f xs -> match xs with
        | [] -> pp f ""
        | x::r -> pp f "%a%a" ppr x (print_list' ppr) r

and print_ctyps f xs = print_list print_ctyp f xs

and print_constraint f = function
    | (ctyp1, ctyp2) -> pp f "<Constraint>%a%a</Constraint>" print_ctyp ctyp1 print_ctyp ctyp2

and print_constraints f = print_list print_constraint f

and print_option_ident f = function
    | ONone -> pp f "<None/>"
    | OSome(x) -> pp f "<Some>%a</Some>" print_ident x
    | OAnt(str) -> pp f "<Antiquotation>%s</Antiquotation>" (escape_string str)
