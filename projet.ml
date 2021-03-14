
(*Projet de programmation fonctionnelle*)
(*Simplification d'expressions arithmétiques*)
(*Réalisé par Fréjoux Gaëtan && Niord Mathieu*)


(*load*)
#load "expression_scanner.cmo";;
(*open*)
open Expression_scanner;;
open Stack;;
open String;;

(*show*)



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
  let stack = Stack.create() in
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
  |Minus -> if ((c1-c2)<0)
            then Unary(Cst(c1 - c2))
            else Cst(c1 - c2)
  |Mult -> Cst(c1 * c2)
  |Div -> Cst(c1 / c2)
;;

let isPriority op =
  match op with
  | Plus -> false
  | Minus -> false
  | _ -> true
;;


let rec simplifyTree tree =
  match tree with

  (*Cas d'une variable*)
  |Var(x) -> tree
  (*Cas d'une constante*)
  |Cst(c) -> tree
  (*Cas d'une valeur négative*)
  |Unary(t) -> 
    (
      let son = simplifyTree t in
      match son with
      (*Cas d'une valeur négative (--x) -> (x)  *)
      | Unary(v) -> v
      (*Autres cas :*)
      | _ -> Unary(son)
    )
  (*Cas d'une operation :*)
   
  | Binary(op,gauche,droite) ->
     let (eg,ed) = (simplifyTree gauche,simplifyTree droite) in
     match op,eg,ed with
     (*Cas 2 constantes :*)
     | _,Cst(c1),Cst(c2) -> operBinary op c1 c2
     (*Cas (1*x) ou (x*1)*)
     | Mult,g,Cst(1) -> g
     | Mult,Cst(1),d -> d
     (*Cas (0+x) ou (x+O)*)
     | Plus,g,Cst(0) -> g
     | Plus,Cst(0),d -> d
     (*Cas (0-x) ou (x-0)*)
     | Minus,g,Cst(0) -> g
     | Minus,Cst(0),d -> Unary(d)
     (*Cas (0*x) ou (x*0) *)
     | Mult,_,Cst(0) -> Cst(0)
     | Mult,Cst(0),_ -> Cst(0)
     (*Cas (0/x) ou (x/0)*)
     | Div,_,Cst(0) -> failwith "Cannot divide by 0."
     | Div,Cst(0),_ -> Cst(0)
     (*Cas (x/1)*)
     | Div,g,Cst(1) -> g
     (*Cas (x/x)*)
     | Div,g,d -> if (g=d)
                  then Cst(1)
                  else Binary(op,eg,ed)
     (*Cas x - (-y)*)
     | Minus,g,Unary(d) -> Binary(Plus,g,d)
     (*Cas (-x) + y*)
     | Plus,Unary(g),d -> Binary(Minus,d,g)
     (*Cas plus complexe :*)
     (*Cas ((2+x)+3) ou ((x+2)+3)*)
     | Plus,Binary(Plus,Cst(c1),d),Cst(c2) -> Binary(Plus,Cst(c1+c2),d)
     | Plus,Binary(Plus,g,Cst(c1)),Cst(c2) -> Binary(Plus,g,Cst(c1+c2))
     (*Cas (3+(2+x)) ou (3 + (x+2))*)
     | Plus,Cst(c1),Binary(Plus,Cst(c2),d) -> Binary(Plus,Cst(c1+c2),d)
     | Plus,Cst(c1),Binary(Plus,g,Cst(c2)) -> Binary(Plus,g,Cst(c1+c2))
     (*Autres cas*)
     | _ -> Binary(op,eg,ed)
;;

(*TEST*)
(*
  let t = simplifyTree ans;;
 *)

(*Affichage du résultat*)

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


let par s =
  "("^s^")"
;;

let rec displayTree t =
  match t with
  |Var(x) -> string_of_char x
  |Cst(c) -> string_of_int c
  |Unary(t) -> "(-"^(displayTree t)^")"
  |Binary(op, g, d) ->
    let sG = displayTree g and
        sD = displayTree d and
        sO = string_of_op op in
    
    match g,d with
      
    | Binary(opG,_,_),Binary(opD,_,_) ->
       if op = opG && op = opD
       then sG^sO^sD
       else (par sG)^sO^(par sD)

      
    | Binary(opG,_,_),_ ->
       if op = opG
       then sG^sO^sD
       else
         (
           if (isPriority op)
           then (par sG)^sO^sD
           else sG^sO^sD
         )
    | _,Binary(opD,_,_) ->
       if op = opD
       then sG^sO^sD
       else
         (
           if (isPriority op)
           then sG^sO^(par sD)
           else sG^sO^sD
         )
    | _ -> sG^sO^sD 
;;
(*   a*b*c*(e+f)   *)

(*
let tokenL2 = string_to_token_list "a b * c e f + * *;";;
let tree2 = parse tokenL2;;
let simp = simplifyTree tree2;;
let text = displayTree tree2;;

let tokenL3 = string_to_token_list "a b c * +;";;
let tree3 = parse tokenL3;;
let simp2 = simplifyTree tree3;;
let text2 = displayTree tree3;;
 *)


print_endline ("Input here :");;
print_endline (displayTree (simplifyTree (parse (input_to_token_list()))));;
