(* Module de la passe de placement mémoire *)
(* doit être conforme à l'interface Passe *)
open Type 
open Exceptions
open Ast
open Tds
open Tam
open Code

type t1 = Ast.AstPlacement.programme
type t2 = string
let cpt = (1,0)

let n = ref cpt

let initiate n_r = n_r := (1,0);;
let get_g n_r =
  (let (a,_) = !n_r in a);;

let get_d n_r =
  (let (_,b) = !n_r in b);;
let increment_g n_r = (n:= ((get_g n_r)+1,(get_d n_r)));;
let increment_d n_r = (n:= ((get_g n_r),(get_d n_r)+1));;
let decrement_d n_r = 
  if ((get_d n_r)-1 > 0) then 
    (n:= ((get_g n_r),(get_d n_r)-1))
  else
    (n:= ((get_g n_r)+1,0));;

let getType ia =
  match info_ast_to_info ia with
  | InfoVar(_,t,_,_) ->  t
  | InfoConst _ -> Int
  | _ -> failwith "erreur";;

let getDep (InfoVar(_,_,dep,_)) = dep;;

let getReg (InfoVar(_,_,_,reg)) = reg;;

let rec getTypeVal p = 
  match p with 
    |AstType.Ident(inf) -> getType inf 
    |AstType.Valeur(p2) -> 
      (match (getTypeVal p2) with 
      | Pointeur t -> t 
      | _ -> failwith "erreur");;

      let getnameinfo inf = 
      match inf with 
      | InfoConst(n,_) -> n
      | InfoFun(n,_,_) -> n
      | InfoVar(n,_,_,_) -> n
      | InfoEtiq(n) -> n;;
let rec analyse_code_affectable aff =
  (match aff with 
    | AstType.Ident(ia) ->
      (
        Tam.store (getTaille (getType ia)) (getDep (info_ast_to_info ia)) (getReg (info_ast_to_info ia))
      )

    | AstType.Valeur p -> 
      analyse_code_affectable p 
      ^ Tam.storei (getTaille (getTypeVal aff)))

and analyse_code_instruction iaop i =
  match i with

   | AstPlacement.Empty -> ""

   | AstPlacement.Declaration(ia,e) -> 
    (Tam.push (getTaille(getType ia))
    ^ analyse_code_expression e
    ^ Tam.store (getTaille(getType ia)) (getDep(info_ast_to_info ia)) (getReg(info_ast_to_info ia)))

   | AstPlacement.Affectation(aff,e) ->
    (
      analyse_code_expression e
      ^ analyse_code_affectable aff
      
    )


   | AstPlacement.Conditionnelle(e,b1,b2) ->
    (let etiqSinon = getEtiquette() in 
    let etiqFin = getEtiquette() in 
    analyse_code_expression e
    ^ Tam.jumpif 0 etiqSinon
    ^ analyse_code_bloc b1 iaop
    ^ Tam.jump etiqFin
    ^ etiqSinon ^ "\n"
    ^ analyse_code_bloc b2 iaop
    ^ etiqFin ^ "\n")

   | AstPlacement.ConditionnelleSansElse(e,b) -> 

    (let etiqFin = getEtiquette() in 
    analyse_code_expression e
    ^ Tam.jumpif 0 etiqFin
    ^ analyse_code_bloc b iaop
    ^ etiqFin ^ "\n")

   | AstPlacement.TantQue(e,b) -> 
    (let etiqDebut = getEtiquette() in
    let etiqFin = getEtiquette() in
    etiqDebut ^ "\n"
    ^ analyse_code_expression e
    ^ Tam.jumpif 0 etiqFin
    ^ analyse_code_bloc b iaop
    ^ Tam.jump etiqDebut
    ^ etiqFin ^ "\n")

   | AstPlacement.Loop b ->
    (increment_d n);
    (let etiqdebut = ("LoopStart_"^(string_of_int (get_g n)) ^ (string_of_int (get_d n))) in
    let etiqfin = ("LoopEnd_"^(string_of_int (get_g n)) ^ (string_of_int (get_d n))) in
    etiqdebut ^ "\n"
    ^ analyse_code_bloc b iaop
    ^ Tam.jump etiqdebut 
    ^ etiqfin ^ "\n")

   | AstPlacement.Break ->
    (match iaop with
    |None ->
      (let ret = (Tam.jump "LoopEnd_" ^ (string_of_int (get_g n)) ^ (string_of_int (get_d n))) in 
      (decrement_d n);
      ret)
    |Some ia -> Tam.jump ( "LoopEnd_"^getnameinfo (info_ast_to_info ia)))
    
    | AstPlacement.BreakId ia ->
     Tam.jump ( "LoopEnd_"^getnameinfo (info_ast_to_info ia))

   | AstPlacement.AffichageBool e ->
    ((analyse_code_expression e
     ^ Tam.subr "BOut"))

   | AstPlacement.AffichageInt e ->
    ((analyse_code_expression e
     ^ Tam.subr "IOut"))

   | AstPlacement.AffichageRat e ->
    ((analyse_code_expression e
     ^ Tam.call "ST" "ROut"))

   | AstPlacement.Retour(e,tr,tp) ->
    ((analyse_code_expression e
     ^ Tam.return tr tp))

   | AstPlacement.LoopId (ia,blc) ->
    (let idloop = getnameinfo(info_ast_to_info ia) in 
    let etiqdebut = "LoopStart_"^idloop in
    let etiqfin =  "LoopEnd_"^idloop in
    etiqdebut ^ "\n"
    ^ analyse_code_bloc blc (Some ia)
    ^ Tam.jump etiqdebut
    ^ etiqfin^ "\n")


   | AstPlacement.ContinueId ia -> 
    Tam.jump ("LoopStart_"^getnameinfo (info_ast_to_info ia))

   | AstPlacement.Continue ->
    (match iaop with
    |None ->
    ( Tam.jump "LoopStart_" ^ (string_of_int (get_g n)) ^ (string_of_int (get_d n)))
    |Some ia -> Tam.jump ("LoopStart_"^getnameinfo (info_ast_to_info ia)))



and analyse_code_bloc (li,taille) iaop =
    String.concat " " (List.map (analyse_code_instruction iaop) li)
    ^ Tam.pop 0 taille

and analyse_code_expression e =
    match e with
     |AstType.Booleen(b) -> 
      if b then
        Tam.loadl_int 1
      else
        Tam.loadl_int 0
     |AstType.Entier(n) -> Tam.loadl_int n
     |AstType.Unaire(u,e) ->
      begin
        analyse_code_expression e
        ^ match u with
          |Numerateur -> Tam.pop 0 1
          |Denominateur -> Tam.pop 1 1
      end
     |AstType.Binaire(b,e1,e2) ->
      begin
        analyse_code_expression e1 ^ analyse_code_expression e2
        ^ match b with
         |Fraction -> Tam.call "LB" "norm"
         |PlusInt -> Tam.subr "IAdd"
         |PlusRat -> Tam.call "LB" "RAdd"
         |MultInt -> Tam.subr "IMul"
         |MultRat -> Tam.call "LB" "RMul"
         |Inf -> Tam.subr "ILss"
         |EquBool -> Tam.subr "IEq"
         |EquInt -> Tam.subr "IEq"
      end
     |AstType.Affectable aff -> 
      (match aff with
        |Ident(ia) -> 
          begin
            match info_ast_to_info ia with
              |InfoVar(_,t,dep,reg) -> Tam.load (getTaille t) dep reg
              |InfoConst(_,n) -> Tam.loadl_int n
              |_ -> failwith " "
          end
        |Valeur aff2 -> (analyse_code_affectable aff2)  ^ Tam.loadi (getTaille (getTypeVal aff)))

     |AstType.New t -> (Tam.loadl_int (getTaille t)) ^ Tam.subr "MAlloc"
     
     |AstType.Addresse ia -> 
      (match info_ast_to_info ia with
        |InfoVar(_,_,dep,reg) -> Tam.loada dep reg
        |_ -> failwith " ")
     |AstType.Null -> ""

     |AstType.Ternaire(e1,e2,e3) -> 
      (let etiq2 = getEtiquette() in 
      let etiqFin = getEtiquette() in 
        analyse_code_expression e1 
        ^ Tam.jumpif 0 etiq2
        ^ analyse_code_expression e2
        ^ Tam.jump etiqFin
        ^ etiq2 ^ "\n"
        ^ analyse_code_expression e3
        ^ etiqFin ^ "\n")

     |AstType.AppelFonction(ia,l) ->
      (match info_ast_to_info ia with
       |InfoFun(n,_,_) -> String.concat " " (List.map analyse_code_expression l) ^ Tam.call "ST" n
       |_ -> failwith " ")
and getinforfunname inf =
  match info_ast_to_info inf with
    | InfoFun(n,_,_) -> n
    | _ -> failwith "erreur"
and analyse_code_fonction iaop (AstPlacement.Fonction(ia,_,b)) =
getinforfunname ia ^ "\n"
^ analyse_code_bloc b iaop
^ Tam.halt

and analyser (AstPlacement.Programme (fonctions,prog)) =
  getEntete()
  ^ String.concat "" (List.map (analyse_code_fonction None) fonctions)
  ^ "main\n"
  ^ analyse_code_bloc prog None
  ^ Tam.halt