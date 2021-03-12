(*Projet de programmation fonctionnelle*)
(*Simplification d'expressions arithmÈtiques*)
(*RÈalisÈ par FrÈjoux GaÎtan && Niord Mathieu*)


(*load*)
#load "expression_scanner.cmo";;

(*open*)
open Expression_scanner;;
open Stack;; (*Module Pile*)

(*show*)
#show Expression_scanner;;
#show Stack;;





(*Types*)

(*operator*)
type operator = | Plus | Minus | Mult | Div;;

(*tree*)
type tree =
  | Var of char
  | Cst of int
  | Unary of tree
  | Binary of operator * tree * tree
;;




(*tests*)
let x = string_to_token_list " 34 56 2 + x * -;";;


(*Analyse lexicale*)

(*Cette partie est donn√©e dans le sujet en utilisant le module expression_scanner*)






(*Analyse syntaxique et construction de l'arbre*)

(*TODO*)

(*fonctions*)

(*Fonction qui permet de transformer un token en operateur*)
let tokenToOperator token =
  match token with
  | Add -> Plus
  | Subtract -> Minus
  | Multiply -> Mult
  | Divide -> Div
  | _ -> failwith" It's not an operator."
;;

(*fonction qui transforme une liste de token en un arbre de syntaxe abstraite*)
let parse list =
  
  let ended = ref false and (*booleen qui dit si l'expression s'est termin√© par ';' *)
      stack_len = ref 0 and (*Longueur de la pile*)
      error = "This expression is not valid." in (*Message d'erreur*)
  
  let rec parse_aux list stack = (*Fonction anonyme*)
    if(!stack_len<0) 
    then failwith error
    else
      (
        match list with
          
        | [] ->
           if (!ended && !stack_len = 1) (*Test finir par End et taille = 1*)
           then ()
           else failwith error
          
        | hd::tl ->
           ignore(
               match hd with

               | Variable(v) -> (*Cas variable*)
                  
                  stack_len := !stack_len + 1;
                  push (Var(v)) stack
                  
               | Number(n) -> (*Cas nombre*)
                  
                  stack_len := !stack_len + 1;
                  push (Cst(n)) stack;
                  
               | Minus -> (*Cas de l'operateur unaire*)
                  
                  if(!stack_len<1)
                  then failwith error
                  else push (Unary(pop stack)) stack
                 

               | End -> (*Cas de la fin de l'expression*)
                  
                  if(tl=[])
                  then ended := true
                  else failwith error
                 
               | _ -> (* Cas des operateurs *)
                  
                  if(!stack_len<2) (*test si il y a le fils gauche et droit*)
                  then failwith error
                  else
                    (
                      stack_len := !stack_len - 1; (*retire 2 ÈlÈment et en rajoute 1 donc -1*)
                      let d = pop stack in (*fils droit*)
                      let g = pop stack in (*fils gauche*)
                      let op = tokenToOperator hd in (*opeateur*)
                      
                      push (Binary(op,g,d)) stack 

                    )
             );
           parse_aux tl stack
      )
  in
  let stack = create() in (*Pile vide*)
  parse_aux list stack; (*Pile ayant pour seul ÈlÈment l'arbre.*)
  pop stack (*renvoie l'arbre.*)
;;

let ans = parse x;;






(*Simplication sur l'arbre*)

(*TODO*)

(*fonction qui simplifie un arbre de syntaxe abstraite*)






(*Affichage du r√©sultat*)

(*TODO*)

(*fonctionn qui transforme un arbre de syntaxe abstraite en un string*)
