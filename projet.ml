(*Projet de programmation fonctionnelle*)
(*Simplification d'expressions arithm�tiques*)
(*R�alis� par Fr�joux Ga�tan && Niord Mathieu*)


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
let x = string_to_token_list " 34 ~ 56 2 + x * -;";;


(*Analyse lexicale*)

(*Cette partie est donn�e dans le sujet en utilisant le module expression_scanner*)




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
  | _ -> failwith "--- Error : It's not an operator ---"
;;

(*fonction qui transforme une liste de token en un arbre de syntaxe abstraite*)
let parse list =
  
  let ended = ref false and (*booleen qui indique si l'expression s'est termin�e par ';' *)
      stack_len = ref 0 and (*Longueur de la pile*)
      error = "--- Invalid Expression ---" in (*Message d'erreur*)
  
  let rec parse_aux list stack = (*Fonction anonyme*)
    if(!stack_len < 0) 
    then failwith error
    else
      (
        match list with
          
        |[] -> if (!ended && !stack_len = 1) (*Test finir par End et taille = 1*)
               then ()
               else failwith error
         
        |hd::tl -> ignore(
                       match hd with

                       (*Cas variable*)
                       |Variable(v) -> stack_len := !stack_len + 1;
                                       push (Var(v)) stack

                       (*Cas nombre*)
                       |Number(n) -> stack_len := !stack_len + 1;
                                     push (Cst(n)) stack;

                       (*Cas de l'operateur unaire*)
                       |Minus -> if(!stack_len<1)
                                 then failwith error
                                 else push (Unary(pop stack)) stack
                        
                       (*Cas de la fin de l'expression*)
                       |End -> if(tl=[])
                               then ended := true
                               else failwith error
                             
                       (* Cas des operateurs *)
                       |_ -> if(!stack_len<2) (*test si il y a le fils gauche et droit*)
                             then failwith error
                             else
                               (
                                 stack_len := !stack_len - 1; (*retire 2 �l�ment et en rajoute 1 donc -1*)
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
  parse_aux list stack; (*Pile ayant pour seul �l�ment l'arbre.*)
  pop stack (*renvoie l'arbre.*)
;;

let ans = parse x;;



(*Simplication sur l'arbre*)

(*fonction qui simplifie un arbre de syntaxe abstraite*)
(*Dans cette fonction de simplification, il y a 4 cas possible pour simplifier :

  -  une sous-expression constitu�e exclusivement de constantes.

  -  une expression de la forme 1*x ou 0 + x sera simplifi�e en x ; de m�me l'expression 0 * x
     sera simplifi�e par 0

  -  une sous-expression de la forme 0 * x sera simplifi�e en 0 

  -  une sous-expression de la forme x/x sera simplifi�e en 1
 *)


let simplifyTree tree =

(*TODO*)
  
;;




(*Affichage du r�sultat*)

(*TODO*)

(*fonctionn qui transforme un arbre de syntaxe abstraite en un string*)
(*Dans cette fonction d'affichage, il faudra afficher l'expression avec le moins de parenth�ses
 possible.
 
  -  Ne pas mettre de parenth�se lorsqu'il y a association : ((a*b)*c)*(e+f) doit �tre a*b*c*(e+f)
 *)
