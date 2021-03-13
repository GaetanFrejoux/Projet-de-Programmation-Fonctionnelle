(*Projet de programmation fonctionnelle*)
(*Simplification d'expressions arithmétiques*)
(*Réalisé par Fréjoux Gaëtan && Niord Mathieu*)


(*load*)
#load "expression_scanner.cmo";;

(*open*)
open Expression_scanner;;
open Stack;; (*Module Pile*)
open String;;

(*show*)
#show Expression_scanner;;
#show Stack;;
#show String;;


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

(*Cette partie est donnée dans le sujet en utilisant le module expression_scanner*)




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
  
  let ended = ref false and (*booleen qui indique si l'expression s'est terminée par ';' *)
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
                                 stack_len := !stack_len - 1; (*retire 2 élément et en rajoute 1 donc -1*)
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
  parse_aux list stack; (*Pile ayant pour seul élément l'arbre.*)
  pop stack (*renvoie l'arbre.*)
;;

let ans = parse x;;



(*Simplication sur l'arbre*)

(*fonction qui simplifie un arbre de syntaxe abstraite*)
(*Dans cette fonction de simplification, il y a 4 cas possible pour simplifier :

  -  une sous-expression constituée exclusivement de constantes.

  -  une expression de la forme 1*x ou 0 + x sera simplifiée en x ; de même l'expression 0 * x
     sera simplifiée par 0

  -  une sous-expression de la forme 0 * x sera simplifiée en 0 

  -  une sous-expression de la forme x/x sera simplifiée en 1
 *)

let operBinary op c1 c2 =
  match op with
  |Plus -> Cst(c1 + c2)
  |Minus -> Cst(c1 - c2)
  |Mult -> Cst(c1 * c2)
  |Div -> Cst(c1/c2)
;;


let rec simplifyTree tree =
  match tree with
    
  |Var(x) -> tree
           
  |Cst(c) -> tree
           
  |Unary(t) -> let son = simplifyTree t in
               Unary(son)
          
  (*- Opération sur deux constantes -*)
  |Binary(op, Cst(c1), Cst(c2)) -> operBinary op c1 c2
                                 
  (*- 1 * x -*)
  |Binary(Mult, Var(x), Cst(1)) -> Var(x)
  |Binary(Mult, Cst(1), Var(x)) -> Var(x)
                                 
  (*- 0 + x -*)
  |Binary(Plus, Var(x), Cst(0)) -> Var(x)
  |Binary(Plus, Cst(0), Var(x)) -> Var(x)
                                 
  (*- 0 * x -*)
  |Binary(Mult, Var(x), Cst(0)) -> Cst(0)
  |Binary(Mult, Cst(0), Var(x)) -> Cst(0)
                                 
  (*- x/x -*)
  |Binary(Div, Var(x1), Var(x2)) -> if x1 = x2
                                   then Cst(1)
                                   else tree
                                  
  |Binary(op, g, d) -> Binary(op, simplifyTree g, simplifyTree d)
;;

(*TEST*)
let t = simplifyTree ans;;


(*Affichage du résultat*)

(*TODO*)

(*fonction qui transforme un arbre de syntaxe abstraite en un string*)
(*Dans cette fonction d'affichage, il faudra afficher l'expression avec le moins de parenthèses
 possible.
 
  -  Ne pas mettre de parenthèse lorsqu'il y a association : ((a*b)*c)*(e+f) doit être a*b*c*(e+f)
 *)

let string_of_char c =
  make 1 c
;;

let string_of_op op =
  match op with
  |Plus -> " + "
  |Minus -> " - "
  |Mult -> " * "
  |Div -> "/"
;;

let rec displayTree t =
  match t with
  |Var(x) -> string_of_char x
  |Cst(c) -> string_of_int c
  |Unary(t) -> "-"^(display_aux t)
  |Binary(op, g, d) -> let res = (display_aux g)^(string_of_op op)^(display_aux d) in
                       match op with
                       |Div
                        |Mult
                       
;;

displayTree t;;

string_of_char 'g';;
