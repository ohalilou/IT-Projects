open Tds
open Exceptions
open Ast
open Type


  type t1 = Ast.AstTds.programme
  type t2 = Ast.AstType.programme


let gettype inf =
  match inf with
  | InfoConst(_,_) -> Type.Int
  | InfoVar(_,t,_,_) -> t
  | _ -> failwith "no retrievable type"

let gettypefun inf =
  match inf with 
  | InfoFun(_,retour,params) -> retour,params
  | _ -> failwith "no function type retrievable"


let rec analyser_type_affectable aff = 
  match aff with 
  |AstTds.Ident(inf) -> AstType.Ident(inf),gettype (info_ast_to_info inf)
  |AstTds.Valeur (p) -> 
    let np,tp = analyser_type_affectable p in 
    match tp with 
    |Pointeur t -> AstType.Valeur(np), t
    | _ -> raise (TypeInattendu(tp,Pointeur Undefined))

let rec analyser_type_expression expr =
  (match expr with
  | Ast.AstTds.AppelFonction(inf,lp) ->
    (let nlp = List.map (analyser_type_expression) lp in 
    let lt = List.map snd nlp in 
    let lpe = List.map fst nlp in
    let ltr,ltv = (gettypefun (info_ast_to_info inf)) in
    if (est_compatible_list lt ltv) then
      AstType.AppelFonction(inf, lpe), ltr
    else
      raise (TypesParametresInattendus(lt,ltv)))

  | AstTds.Affectable aff -> 
    let naff,taff = analyser_type_affectable aff in
    (AstType.Affectable naff), taff

  | AstTds.Booleen(truth) ->AstType.Booleen(truth), Bool

  | AstTds.Entier(nb) -> AstType.Entier(nb),Int

  | AstTds.Unaire(unaire, expr) -> 
    (let ne,nt = analyser_type_expression expr in
    if nt = Rat then
      (match unaire with 
      | AstSyntax.Numerateur -> AstType.Unaire(AstType.Numerateur, ne),Int 
      | AstSyntax.Denominateur -> AstType.Unaire(AstType.Denominateur, ne),Int)
    else
      raise (TypeInattendu(nt,Rat)))

  | AstTds.Binaire(bin,expr1,expr2) -> 
    (let ne1,nt1 = analyser_type_expression expr1 in 
     let ne2,nt2 = analyser_type_expression expr2 in
     if not (est_compatible nt1 nt2) then
      raise (TypeBinaireInattendu(bin,nt1,nt2))
     else
      (match nt1 with

      | Bool -> 
        (match bin with 
        | AstSyntax.Equ -> AstType.Binaire(AstType.EquBool,ne1,ne2),Bool
        | _ -> raise (TypeBinaireInattendu(bin,nt1,nt2)))

      | Int -> 
        (match bin with
        | AstSyntax.Equ -> AstType.Binaire(AstType.EquInt, ne1,ne2),Bool
        | AstSyntax.Plus -> AstType.Binaire(AstType.PlusInt, ne1, ne2), Int
        | AstSyntax.Mult -> AstType.Binaire(AstType.MultInt, ne1,ne2), Int
        | AstSyntax.Inf -> AstType.Binaire(AstType.Inf, ne1,ne2), Bool
        | AstSyntax.Fraction -> AstType.Binaire(AstType.Fraction, ne1, ne2), Rat)

      | Rat ->
        (match bin with
        | AstSyntax.Plus -> AstType.Binaire(AstType.PlusRat, ne1, ne2), Rat
        | AstSyntax.Mult -> AstType.Binaire(AstType.MultRat, ne1,ne2), Rat
        | _ -> raise (TypeBinaireInattendu(bin,nt1,nt2)))

      | Pointeur _ -> raise (TypeBinaireInattendu(bin,nt1,nt2))

      | Undefined -> raise (TypeBinaireInattendu(bin,nt1,nt2))))

  | AstTds.Ternaire(e1,e2,e3) -> 
    let ne1,te1 = analyser_type_expression e1 in
    let ne2,te2 = analyser_type_expression e2 in
    let ne3,te3= analyser_type_expression e3 in
    (if not (est_compatible te1 Bool) then
      raise (TypeInattendu(te1,Bool))
    else
      if not (est_compatible te2 te3) then
        raise (TypeTernaireInattendu(te2,te3))
      else
        AstType.Ternaire(ne1,ne2,ne3), te2)
                                    
  | AstTds.Addresse inf ->   AstType.Addresse(inf), Pointeur (gettype (info_ast_to_info inf))                             
  | AstTds.New t -> (AstType.New t), (Pointeur t)
  | AstTds.Null -> AstType.Null, (Pointeur Undefined))                        
                                    
                               
and analyser_type_instruction instr =


    match instr with 

    
    | AstTds.Declaration(tp,inf,expr) -> 
      ((Tds.modifier_type_variable tp inf);
        let ne,nt = analyser_type_expression expr in
        if (est_compatible tp nt) then
          AstType.Declaration(inf,ne)
        else
          raise (Exceptions.TypeInattendu(nt,tp)))


    | AstTds.Affectation(aff,expr) -> 
      ( let naff, taff = analyser_type_affectable aff in
        let ne,te = analyser_type_expression expr in
        (*(print_endline ((string_of_type taff)^" type de l'affectable"));
        (print_endline ((string_of_type te)^" type de l'expr à affecter"));*)
        if est_compatible taff te then
          AstType.Affectation(naff,ne)
        else
          raise (Exceptions.TypeInattendu(te, taff))) 


    | AstTds.Affichage expr -> 
        (let e,t = analyser_type_expression expr in
          match t with 
          | Type.Int -> AstType.AffichageInt e
          | Type.Bool -> AstType.AffichageBool e
          | Type.Rat -> AstType.AffichageRat e
          | Type.Pointeur tt -> raise (TypeInattendu(t,tt))
          | Type.Undefined -> failwith "undefined type")


    | AstTds.Conditionnelle(cond,bt,be) -> 
      (let nc,tp = analyser_type_expression cond in
        if (est_compatible Bool tp) then
          AstType.Conditionnelle(nc, analyser_type_bloc bt, analyser_type_bloc be)
        else
          raise (TypeInattendu(tp, Bool)))


    | AstTds.ConditionnelleSansElse(cond,b) -> 
      (let nc,tp = analyser_type_expression cond in
        if (est_compatible Bool tp) then
          AstType.ConditionnelleSansElse(nc, analyser_type_bloc b)
        else
          raise (TypeInattendu(tp, Bool)))

    | AstTds.Loop b -> AstType.Loop (analyser_type_bloc b)
    | AstTds.LoopId(inf,b) -> AstType.LoopId(inf,analyser_type_bloc b)
    | AstTds.Continue -> AstType.Continue
    | AstTds.ContinueId inf -> AstType.ContinueId inf 
    | AstTds.Break -> AstType.Break
    | AstTds.BreakId inf -> AstType.BreakId inf
    
    
    | AstTds.TantQue(expr,bloc) ->
      (let nexpr,nt = analyser_type_expression expr in
      (if (est_compatible Bool nt) then 
        AstType.TantQue(nexpr,(analyser_type_bloc bloc))
      else
        raise (TypeInattendu(nt, Bool))))

    
    | AstTds.Retour(expr,inf) -> 
      (let ne,nt = analyser_type_expression expr in 
      (if (est_compatible nt (fst (gettypefun (info_ast_to_info inf)))) then 
        AstType.Retour(ne,inf)
      else
        raise (TypeInattendu(nt,(fst (gettypefun (info_ast_to_info inf)))))))


    | Empty -> Empty



and analyser_type_bloc blc = List.map analyser_type_instruction blc


and getnameinfo inf = 
match inf with 
| InfoConst(n,_) -> n
| InfoFun(n,_,_) -> n
| InfoVar(n,_,_,_) -> n
| InfoEtiq(n) -> n

and analyser_type_fonction (AstTds.Fonction(tp,infst,nfli,blc)) =
let nvblc = analyser_type_bloc blc in
let nom = getnameinfo (info_ast_to_info infst) in 
AstType.Fonction(info_to_info_ast (InfoFun(nom,tp,(List.map (fst) nfli))), (List.map (snd) nfli), nvblc)
;;


let analyser (AstTds.Programme (fonctions,prog)) =
    let nf = List.map (analyser_type_fonction) fonctions in
    let nb = analyser_type_bloc prog in
    AstType.Programme (nf,nb)