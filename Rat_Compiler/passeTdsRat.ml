(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast

type t1 = Ast.AstSyntax.programme
type t2 = Ast.AstTds.programme

(* analyse_tds_expression : tds -> AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_expression tds e = 
  match e with
  |AstSyntax.AppelFonction (f,lp) -> 
    begin
      match chercherGlobalement tds (f^"_id") with
      | None -> (*fonction non définie*)
        raise (IdentifiantNonDeclare f)
      | Some info -> 
        begin
          match info_ast_to_info info with
          | InfoFun _ ->
            (* Vérification de la bonne utilisation des identifiants dans l'expression *)
            (* et obtention de l'expression transformée *)
            let liste_np = List.map (fun e -> analyse_tds_expression tds e) lp in
            (* Renvoie de la nouvelle affectation où le nom a été remplacé par l'information
               et l'expression remplacée par l'expression issue de l'analyse *)
            AstTds.AppelFonction(info, liste_np)
          |  _ ->
            (* Modification d'une constante ou d'une fonction *)
            raise (MauvaiseUtilisationIdentifiant f)
        end
    end


  |AstSyntax.Affectable aff -> AstTds.Affectable(analyse_tds_affectable tds aff)


  |AstSyntax.Booleen b -> AstTds.Booleen b


  |AstSyntax.Entier i -> AstTds.Entier i


  |AstSyntax.Unaire (u,e) -> AstTds.Unaire (u,(analyse_tds_expression tds e))


  |AstSyntax.Binaire (b,e1,e2) -> AstTds.Binaire(b,(analyse_tds_expression tds e1),(analyse_tds_expression tds e2))


  |AstSyntax.Ternaire (e1,e2,e3) -> AstTds.Ternaire((analyse_tds_expression tds e1), (analyse_tds_expression tds e2), (analyse_tds_expression tds e3))


  |AstSyntax.New t -> AstTds.New t


  |AstSyntax.Addresse a -> 
    begin
      match chercherGlobalement tds (a^"_id") with
      | None -> 
      (* L'identifiant n'est pas trouvé dans la tds locale,
      il n'a donc pas été déclaré dans le bloc courant*)
      raise (IdentifiantNonDeclare a)

      | Some ia -> 
        (* L'identifiant est trouvé dans la tds locale, il a donc déjà été déclaré dans le bloc courant *)
        match info_ast_to_info ia with
        |InfoVar _ -> AstTds.Addresse(ia)
        |_ ->  raise (MauvaiseUtilisationIdentifiant a)               
    end
  |AstSyntax.Null -> AstTds.Null 

                

and analyse_tds_affectable tds (aff) =
  match aff with
  |AstSyntax.Valeur (affec) -> (AstTds.Valeur (analyse_tds_affectable tds affec))
  |AstSyntax.Ident x -> 
    begin
      match chercherGlobalement tds (x^"_id") with
      | None -> 
        (* L'identifiant n'est pas trouvé dans la tds locale,
        il n'a donc pas été déclaré dans le bloc courant*)
        raise (IdentifiantNonDeclare x)

      | Some ia -> 
        (* L'identifiant est trouvé dans la tds locale,
        il a donc déjà été déclaré dans le bloc courant *)
        match info_ast_to_info ia with
        | InfoVar _ | InfoConst _-> AstTds.Ident (ia)
        | _ ->  raise (MauvaiseUtilisationIdentifiant x)
                  
      end
(* analyse_tds_instruction : tds -> info_ast option -> AstSyntax.instruction -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si l'instruction i est dans le bloc principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est l'instruction i sinon *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_instruction tds oia i =
  match i with
  | AstSyntax.Declaration (t, n, e) ->
      (begin
        match chercherLocalement tds (n^"_id") with
        | None ->
            (* L'identifiant n'est pas trouvé dans la tds locale,
            il n'a donc pas été déclaré dans le bloc courant *)
            (* Vérification de la bonne utilisation des identifiants dans l'expression *)
            (* et obtention de l'expression transformée *)
            let ne = analyse_tds_expression tds e in
            (* Création de l'information associée à l'identfiant *)
            let info = InfoVar (n,t, 0, "") in
            (* Création du pointeur sur l'information *)
            let ia = info_to_info_ast info in
            (* Ajout de l'information (pointeur) dans la tds *)
            ajouter tds (n^"_id") ia;
            (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information
            et l'expression remplacée par l'expression issue de l'analyse *)
            AstTds.Declaration (t, ia, ne)
        | Some _ ->
            (* L'identifiant est trouvé dans la tds locale,
            il a donc déjà été déclaré dans le bloc courant *)
            raise (DoubleDeclaration n)
      end)

  | AstSyntax.Affectation (aff,e) -> 
    (match aff with
    | Ident n ->
      (match chercherGlobalement tds (n^"_id") with
      | None -> raise (IdentifiantNonDeclare n)
      | Some ia ->
        (match info_ast_to_info ia with
        | InfoConst _ -> raise (MauvaiseUtilisationIdentifiant n)
        | InfoVar _ -> AstTds.Affectation(analyse_tds_affectable tds aff, analyse_tds_expression tds e)
        | _ -> raise (MauvaiseUtilisationIdentifiant n)))
    | Valeur _ -> AstTds.Affectation(analyse_tds_affectable tds aff, analyse_tds_expression tds e))
  
  | AstSyntax.Constante (n,v) ->
     (begin
        match chercherLocalement tds (n^"_id") with
        | None ->
          (* L'identifiant n'est pas trouvé dans la tds locale,
             il n'a donc pas été déclaré dans le bloc courant *)
          (* Ajout dans la tds de la constante *)
          ajouter tds (n^"_id") (info_to_info_ast (InfoConst (n,v)));
          (* Suppression du noeud de déclaration des constantes devenu inutile *)
          AstTds.Empty
        | Some _ ->
          (* L'identifiant est trouvé dans la tds locale,
          il a donc déjà été déclaré dans le bloc courant *)
          raise (DoubleDeclaration n)
      end)

  | AstSyntax.Affichage e ->
      ((* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
      let ne = analyse_tds_expression tds e in
      (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
      AstTds.Affichage (ne))

  | AstSyntax.ConditionnelleSansElse (e,b) -> AstTds.ConditionnelleSansElse(analyse_tds_expression tds e, analyse_tds_bloc tds oia b)
  
  | AstSyntax.Conditionnelle (c,t,e) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc then *)
      let tast = analyse_tds_bloc tds oia t in
      (* Analyse du bloc else *)
      let east = analyse_tds_bloc tds oia e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      AstTds.Conditionnelle (nc, tast, east)

  | AstSyntax.Loop b -> AstTds.Loop (analyse_tds_bloc tds oia b)

  | AstSyntax.LoopId (id,b) -> 
    (match chercherGlobalement tds (id^"_loop") with
    |None -> 
      let tdsf = creerTDSFille tds in
      let infloop = (info_to_info_ast (InfoEtiq(id))) in 
      (*Infoloop est un type d'inforécemment ajouté qui caractérisent les loop à etiquettes*)
      ajouter tdsf (id^"_loop") infloop;
      AstTds.LoopId(infloop,analyse_tds_bloc tdsf oia b)
      
    |Some _ -> raise (ErreurMasquage(id)))

  | AstSyntax.Continue -> AstTds.Continue
  
  | AstSyntax.ContinueId id -> 
    (match chercherGlobalement tds (id^"_loop") with
    |None -> raise (IdentifiantNonDeclare(id))
    |Some ia -> AstTds.ContinueId(ia))
    
  | AstSyntax.Break -> AstTds.Break

  |  AstSyntax.BreakId (id) -> 
    (match chercherGlobalement tds (id^"_loop") with
    |None -> raise (IdentifiantNonDeclare(id))
    |Some ia ->
      (AstTds.BreakId(ia)))

  | AstSyntax.TantQue (c,b) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc *)
      let bast = analyse_tds_bloc tds oia b in
      (* Renvoie la nouvelle structure de la boucle *)
      AstTds.TantQue (nc, bast)

  | AstSyntax.Retour (e) ->
      begin
      (* On récupère l'information associée à la fonction à laquelle le return est associée *)
      match oia with
        (* Il n'y a pas d'information -> l'instruction est dans le bloc principal : erreur *)
      | None -> raise RetourDansMain
        (* Il y a une information -> l'instruction est dans une fonction *)
      | Some ia ->
        (* Analyse de l'expression *)
        let ne = analyse_tds_expression tds e in
        AstTds.Retour (ne,ia)
      end


(* analyse_tds_bloc : tds -> info_ast option -> AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si le bloc li est dans le programme principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est le bloc li sinon *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds oia li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc.
     Cette tds est modifiée par effet de bord *)
   let nli = List.map (analyse_tds_instruction tdsbloc oia) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli


(* analyse_tds_fonction : tds -> AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme la fonction
en une fonction de type AstTds.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_fonction maintds (AstSyntax.Fonction(t,n,lp,li))  =
  match chercherGlobalement maintds (n^"_id") with
  |None -> 
    let infoParam t id = 
     let inf = InfoVar(id,t,0,"") in
     info_to_info_ast inf

    in 
    let lnp = List.map (fun (tp,ep) -> (tp, infoParam tp ep)) lp in
            (* Création de l'information associée à l'identfiant *)
            let list_typ_param = List.map fst lp in
            let info = InfoFun (n,t,list_typ_param ) in
            (* Création du pointeur sur l'information *)
            let ia = info_to_info_ast info in
            (* Ajout de l'information (pointeur) dans la tds *)
            ajouter maintds (n^"_id") ia;
            let tdsf = creerTDSFille maintds in 
            let rec infosf liste_ident liste_info = 
               match (liste_ident,liste_info) with
               |[], _ -> []
               |_, [] -> []
               |(_,id)::q1, (_,i)::q2 -> (id,i)::(infosf q1 q2) 
            and ajouter_si_nouveau tds nom i =  
               match chercherLocalement tds (nom^"_id") with
                |None -> ajouter tds (nom^"_id") i
                |Some _ -> raise (DoubleDeclaration nom) (*cas de double declaration de paramétres*)
            in
            List.iter (fun (id,i) -> ajouter_si_nouveau tdsf id i) (infosf lp lnp);
            (*cas de fonction récursive*)
            ajouter tdsf n ia;
            (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information
            et l'expression remplacée par l'expression issue de l'analyse *)
            AstTds.Fonction (t, ia, lnp ,analyse_tds_bloc tdsf (Some ia) li)
  | Some _ ->
   (* L'identifiant est trouvé dans la tds locale,
     il a donc déjà été déclaré dans le bloc courant *)
      raise (DoubleDeclaration n)

(* analyser : AstSyntax.programme -> AstTds.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.programme *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyser (AstSyntax.Programme (fonctions,prog)) =
  let tds = creerTDSMere () in
  let nf = List.map (analyse_tds_fonction tds) fonctions in
  let nb = analyse_tds_bloc tds None prog in
  AstTds.Programme (nf,nb);;