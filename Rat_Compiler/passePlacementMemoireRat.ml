open Tds
open Exceptions
open Ast
open Type


  type t1 = Ast.AstType.programme
  type t2 = Ast.AstPlacement.programme


  let rec gettype inf =
    match inf with
    | Tds.InfoConst(_,_) -> Type.Int
    | Tds.InfoVar(_,t,_,_) -> t
    | _ -> failwith "no retrievable type"

  and gettypefun inf =
    match inf with 
    | InfoFun(_,retour,params) -> retour,params
    | _ -> failwith "no function type retrievable"

  and gettailletype tp =
    match tp with 
    | Type.Bool -> 1
    | Type.Int -> 1
    | Type.Rat -> 2
    | Type.Pointeur _ -> 1
    | Type.Undefined -> failwith "no data to get size from"

  and gettaille i =
    match i with
    | AstPlacement.Declaration(inf,_) -> gettailletype (gettype (info_ast_to_info inf))
    | _ -> 0

  and taillelst lst =
  match lst with 
  | [] -> 0
  | _::q -> 1 + taillelst q

  and sommelst lst =
  match lst with 
  | [] -> 0
  | i::q -> i + sommelst q


and analyser_dep_affectable affec dep reg = 
  match affec with 
  | AstType.Valeur(aff) -> 
    AstType.Valeur((analyser_dep_affectable aff dep reg))
  | AstType.Ident(inf) -> 
    ((modifier_adresse_variable dep reg inf);
    AstType.Ident(inf))

  and analyser_dep_instruction dep reg i = 
    match i with 
    | AstType.Declaration(inf,exp) -> 
      ( (modifier_adresse_variable dep reg inf);
        AstPlacement.Declaration(inf,exp))


    | AstType.AffichageInt(nb) -> AstPlacement.AffichageInt(nb)


    | AstType.AffichageBool(bl) -> AstPlacement.AffichageBool(bl)


    | AstType.AffichageRat(rt) -> AstPlacement.AffichageRat(rt)


    | AstType.Affectation(aff,exp) ->
      (AstPlacement.Affectation((analyser_dep_affectable aff dep reg), exp))


    | AstType.TantQue(cd,blc) -> 
      ( let blcpl = (analyser_dep_bloc dep reg blc) in
        AstPlacement.TantQue(cd,blcpl))


    | AstType.Conditionnelle(cd,blc1,blc2) -> 
      ( let blcpl1 = analyser_dep_bloc dep reg blc1 in
        let blcpl2 = analyser_dep_bloc dep reg blc2 in
      AstPlacement.Conditionnelle(cd,blcpl1,blcpl2))


    | AstType.Empty -> AstPlacement.Empty


    | AstType.Retour(exp,inf) -> 
      (let rt,param = gettypefun (info_ast_to_info inf) in
      let listetailles = List.map (gettailletype) param in 
      let tailleret = gettailletype rt in
      let tailleparamglo = sommelst listetailles in
      AstPlacement.Retour(exp,tailleret,tailleparamglo))

    | AstType.Break ->AstPlacement.Break
    
    | AstType.Continue -> AstPlacement.Continue

    | AstType.BreakId(inf) -> AstPlacement.BreakId(inf)

    | AstType.ContinueId(inf) -> AstPlacement.ContinueId(inf)

    | AstType.Loop(blc) -> AstPlacement.Loop(analyser_dep_bloc dep reg blc)

    | AstType.LoopId(inf,blc) -> AstPlacement.LoopId(inf, analyser_dep_bloc dep reg blc)
    
    | AstType.ConditionnelleSansElse(exp,blc) -> AstPlacement.ConditionnelleSansElse(exp, analyser_dep_bloc dep reg blc)


  and analyser_dep_bloc add reg blc =
  match blc with 
  | [] -> [],0
  | i::q ->
    (let nins = analyser_dep_instruction add reg i in 
    let lstinstr,taille = (analyser_dep_bloc (add + gettaille(nins)) reg q) in 
    (nins::lstinstr),(taille+gettaille(nins)))

  and analyser_dep_fonctions reg (AstType.Fonction(infof,infosparam,bloc)) = 
  (*mise_a_jour_param : ajout une adresse de l'info inf au registre à partir d'une adresse sommet*)
   let mise_a_jour_param (inf:info_ast) sommet =
    match info_ast_to_info inf with
    | (InfoVar(_,tp,_,_)) -> 
      ( (modifier_adresse_variable (sommet - (gettailletype tp)) reg inf);
      inf, gettailletype tp)
    | _ -> failwith "no updates"
   (*maj_listparam : ajoute les adresses des parametres à partir du sommet du registre LB*)
   in let rec maj_listparam listeinf sommet =
    match listeinf with
    |[] -> []
    |inf::q -> (fst(mise_a_jour_param inf sommet))::(maj_listparam q (sommet - (snd(mise_a_jour_param inf sommet))))
   in let nvlinfosparam =  List.rev(maj_listparam (List.rev infosparam) 0) in 
   AstPlacement.Fonction(infof,nvlinfosparam,analyser_dep_bloc 3 reg bloc)


  and analyser (Ast.AstType.Programme(f,b)) = 
  let nf = (List.map (analyser_dep_fonctions "LB") f) in
  let nblc = analyser_dep_bloc 0 "SB" b in 
  AstPlacement.Programme(nf,nblc);;